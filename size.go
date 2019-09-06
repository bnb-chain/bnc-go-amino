package amino

import (
	"errors"
	"fmt"
	"reflect"
	"time"
)

func (cdc *Codec) sizeBinary(info *TypeInfo, rv reflect.Value, fopts FieldOptions, bare bool) (n int, err error) {
	switch info.Type.Kind() {

	//----------------------------------------
	// Complex

	case reflect.Interface:
		n1, err := cdc.sizeInterface(info, rv, fopts, bare)
		if err != nil {
			return n, err
		}
		n += n1

	case reflect.Array:
		if info.Type.Elem().Kind() == reflect.Uint8 {
			n += sizeBinaryByteArray(info)
		} else {
			n1, err := cdc.sizeBinaryList(info, rv, fopts, bare)
			if err != nil {
				return n, err
			}
			n += n1
		}

	case reflect.Slice:
		if info.Type.Elem().Kind() == reflect.Uint8 {
			n += sizeBinaryByteSlice(info, rv)
		} else {
			n1, err := cdc.sizeBinaryList(info, rv, fopts, bare)
			if err != nil {
				return n, err
			}
			n += n1
		}

	case reflect.Struct:
		n1, err := cdc.sizeStruct(info, rv, fopts, bare)
		if err != nil {
			return n, err
		}
		n += n1

	//----------------------------------------
	// Signed

	case reflect.Int64:
		if fopts.BinFixed64 {
			n += 8
		} else {
			n += UvarintSize(uint64(rv.Int()))
		}

	case reflect.Int32:
		if fopts.BinFixed32 {
			n += 4
		} else {
			n += UvarintSize(uint64(rv.Int()))
		}

	case reflect.Int16:
		if fopts.BinFixed16 {
			n += 2
		} else {
			n += UvarintSize(uint64(rv.Int()))
		}

	case reflect.Int8:
		if fopts.BinFixed8 {
			n++
		} else {
			n += UvarintSize(uint64(rv.Int()))
		}

	case reflect.Int:
		n += UvarintSize(uint64(rv.Int()))

	//----------------------------------------
	// Unsigned

	case reflect.Uint64:
		if fopts.BinFixed64 {
			n += 8
		} else {
			n += UvarintSize(rv.Uint())
		}

	case reflect.Uint32:
		if fopts.BinFixed32 {
			n += 8
		} else {
			n += UvarintSize(rv.Uint())
		}

	case reflect.Uint16, reflect.Uint8, reflect.Uint:
		n += UvarintSize(rv.Uint())

	//----------------------------------------
	// Misc

	case reflect.Bool:
		n++

	case reflect.Float64:
		n += 8

	case reflect.Float32:
		n += 4

	case reflect.String:
		s := rv.String()
		n += UvarintSize(uint64(len(s))) + len(s)

	//----------------------------------------
	// Default
	default:
		panic(fmt.Sprintf("unsupported type %v", info.Type.Kind()))
	}

	return
}

func sizeBinaryByteArray(info *TypeInfo) (n int) {
	ert := info.Type.Elem()
	if ert.Kind() != reflect.Uint8 {
		panic("should not happen")
	}
	length := info.Type.Len()
	return UvarintSize(uint64(length)) + length
}

func sizeBinaryByteSlice(info *TypeInfo, rv reflect.Value) (n int) {
	ert := info.Type.Elem()
	if ert.Kind() != reflect.Uint8 {
		panic("should not happen")
	}
	length := len(rv.Bytes())
	return UvarintSize(uint64(length)) + length
}

func (cdc *Codec) sizeInterface(iinfo *TypeInfo, rv reflect.Value, fopts FieldOptions, bare bool) (n int, err error) {
	// Special case when rv is nil, write 0x00 to denote an empty byteslice.
	if rv.IsNil() {
		n++
		// _, err = w.Write([]byte{0x00})
		return
	}

	// Get concrete non-pointer reflect value & type.
	var crv, isPtr, isNilPtr = derefPointers(rv.Elem())
	if isPtr && crv.Kind() == reflect.Interface {
		// See "MARKER: No interface-pointers" in codec.go
		panic("should not happen")
	}
	if isNilPtr {
		panic(fmt.Sprintf("Illegal nil-pointer of type %v for registered interface %v. "+
			"For compatibility with other languages, nil-pointer interface values are forbidden.", crv.Type(), iinfo.Type))
	}
	var crt = crv.Type()

	// Get *TypeInfo for concrete type.
	var cinfo *TypeInfo
	cinfo, err = cdc.getTypeInfo_wlock(crt)
	if err != nil {
		return
	}
	if !cinfo.Registered {
		err = fmt.Errorf("Cannot encode unregistered concrete type %v.", crt)
		return
	}

	// Write disambiguation bytes if needed.
	if iinfo.AlwaysDisambiguate || len(iinfo.Implementers[cinfo.Prefix]) > 1 {
		n += 1 + len(cinfo.Disamb)
		// _, err = buf.Write(append([]byte{0x00}, cinfo.Disamb[:]...))
	}

	n += len(cinfo.Prefix.Bytes())

	// Write actual concrete value.
	n1, err := cdc.sizeBinary(cinfo, crv, fopts, true)
	if err != nil {
		return n, err
	}
	n += n1

	if !bare {
		n += UvarintSize(uint64(n))
	}
	return
}

func (cdc *Codec) sizeBinaryList(info *TypeInfo, rv reflect.Value, fopts FieldOptions, bare bool) (n int, err error) {
	ert := info.Type.Elem()
	if ert.Kind() == reflect.Uint8 {
		panic("should not happen")
	}
	einfo, err := cdc.getTypeInfo_wlock(ert)
	if err != nil {
		return
	}

	// If elem is not already a ByteLength type, write in packed form.
	// This is a Proto wart due to Proto backwards compatibility issues.
	// Amino2 will probably migrate to use the List typ3.  Please?  :)
	typ3 := typeToTyp3(einfo.Type, fopts)
	if typ3 != Typ3_ByteLength {
		// Write elems in packed form.
		for i := 0; i < rv.Len(); i++ {
			// Get dereferenced element value (or zero).
			var erv, _, _ = derefPointersZero(rv.Index(i))
			n1, err := cdc.sizeBinary(einfo, erv, fopts, false)
			if err != nil {
				return n, err
			}
			n += n1
		}
	} else {
		// NOTE: ert is for the element value, while einfo.Type is dereferenced.
		isErtStructPointer := ert.Kind() == reflect.Ptr && einfo.Type.Kind() == reflect.Struct

		// Write elems in unpacked form.
		for i := 0; i < rv.Len(); i++ {
			// Write elements as repeated fields of the parent struct.
			n += sizeFieldNumberAndTyp3(fopts.BinFieldNum, Typ3_ByteLength)
			// Get dereferenced element value and info.
			var erv, isDefault = isDefaultValue(rv.Index(i))
			if isDefault {
				// Special case if:
				//  - erv is a struct pointer and
				//  - field option has EmptyElements set
				if isErtStructPointer && fopts.EmptyElements {
					// NOTE: Not sure what to do here, but for future-proofing,
					// we explicitly fail on nil pointers, just like
					// Proto3's Golang client does.
					// This also makes it easier to upgrade to Amino2
					// which would enable the encoding of nil structs.
					return n, errors.New("nil struct pointers not supported when empty_elements field tag is set")
				}
				// Nothing to encode, so the length is 0.
				n++
			} else {
				// Write the element value as a ByteLength.
				// In case of any inner lists in unpacked form.
				efopts := fopts
				efopts.BinFieldNum = 1
				n1, err := cdc.sizeBinary(einfo, erv, efopts, false)
				if err != nil {
					return n, err
				}
				n += n1
			}
		}
	}

	if !bare {
		n += UvarintSize(uint64(n))
	}
	return
}

func (cdc *Codec) sizeStruct(info *TypeInfo, rv reflect.Value, fopts FieldOptions, bare bool) (n int, err error) {
	switch info.Type {

	case timeType:
		// Special case: time.Time
		n += sizeTime(rv.Interface().(time.Time))

	default:
		for _, field := range info.Fields {
			// Get type info for field.
			var finfo *TypeInfo
			finfo, err = cdc.getTypeInfo_wlock(field.Type)
			if err != nil {
				return
			}
			// Get dereferenced field value and info.
			var frv = rv.Field(field.Index)
			//var frvIsPtr = frv.Kind() == reflect.Ptr
			var dfrv, isDefault = isDefaultValue(frv)
			if isDefault && !field.WriteEmpty {
				// Do not encode default value fields
				// (except when `amino:"write_empty"` is set).
				continue
			}
			var n1 int
			// isDefault == false
			if field.UnpackedList {
				// Write repeated field entries for each list item.
				n1, err = cdc.sizeBinaryList(finfo, dfrv, field.FieldOptions, true)
				if err != nil {
					return
				}
				n += n1
			} else {
				n += sizeFieldNumberAndTyp3(field.BinFieldNum, typeToTyp3(finfo.Type, field.FieldOptions))
				// Write field value from rv.
				n1, err = cdc.sizeBinary(finfo, dfrv, field.FieldOptions, false)
				if err != nil {
					return
				}
				n += n1
			}
		}
	}

	if !bare {
		n += UvarintSize(uint64(n))
	}
	return
}

func sizeFieldNumberAndTyp3(num uint32, typ Typ3) (n int) {
	// Pack Typ3 and field number.
	var value64 = (uint64(num) << 3) | uint64(typ)
	return UvarintSize(value64)
}

func sizeTime(t time.Time) (n int) {
	s := t.Unix()
	// TODO: We are hand-encoding a struct until MarshalAmino/UnmarshalAmino is supported.
	// skip if default/zero value:
	if s != 0 {
		n += sizeFieldNumberAndTyp3(1, Typ3_Varint) +
			UvarintSize(uint64(s))
	}
	ns := int32(t.Nanosecond()) // this int64 -> int32 cast is safe (nanos are in [0, 999999999])
	// skip if default/zero value:
	if ns != 0 {
		n += sizeFieldNumberAndTyp3(2, Typ3_Varint) +
			UvarintSize(uint64(ns))
	}
	return
}

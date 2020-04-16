package main

import (
    "fmt"
    "encoding/binary"
    "bytes"
)

// 32bit
type header struct {
    Type uint8;
    Code uint8;
    Checksum uint8;
    Id uint8;
}


func main() {
    icmp := header{
        Type: 0xFF, // decimal 255
        Code: 0x80, // decimal 128
        Checksum: 0x10, // decimal 16
        Id: 0x80,
    }

    fmt.Println(icmp)
    fmt.Printf("binary: %b\n", icmp)

    fmt.Println("***** Convert to raw binary *****")
    buf := &bytes.Buffer{}
    binary.Write(buf, binary.BigEndian, icmp)
    fmt.Printf("buf=0x%x\n", buf)

    // convert buf pointer to array of bytes
    fmt.Println("***** Convert rawbuf to buffer array *****")
    bufArr := buf.Bytes()
    for i := 0; i < len(bufArr); i++ {
        fmt.Printf("buffArr[%d]=%x\n", i, bufArr[i])
    }

    // bit shift
    fmt.Println("***** bitshift *****")
    var chksum uint32
    var chksum2 uint32
    var test1 uint16 = 0x80

    // 0xFF + 0x80 + 0x10 + 0x80 = 0x20F = 0d527
    for i := 0; i < len(bufArr); i++ {
        chksum += uint32(bufArr[i])
    }

    // bufArr[i] = 1byte of data
    // so if len(bufArr) = 4, then we have 4bytes = 32bit = uint32
    // So how to convert bufArr to uint32?
    for i := 0; i < len(bufArr); i++ {
        fmt.Printf("len(bufArr)-1+i=%d\n", len(bufArr)-1+i)
        // chksum2 += uint32(bufArr[i]) << uint(len(bufArr) - ((len(bufArr)-1+i) % (len(bufArr) - 1)))
        // chksum2 += uint32(bufArr[i]))
    }

    chksum2 += uint32(bufArr[0]) << 24
    chksum2 += uint32(bufArr[1]) << 16
    chksum2 += uint32(bufArr[2]) << 8
    chksum2 += uint32(bufArr[3]) << 0

    var chksum3 uint32 = uint32(bufArr[0])
    chksum3 = (chksum3 >> 16) + (chksum3 & 0xFFFF)
    chksum3 += (chksum3 >> 16)

    fmt.Println(chksum)
    fmt.Printf("chksum2=%x\n", chksum2)
    fmt.Printf("chksum3=%x\n", chksum3)
    fmt.Println("test1=", test1 >> 4) // 0x80 lsh by 4bit, so it becomes 0x08

    var bit16 uint16 = 0x8A21 // ‭1000 1010 0010 0001‬
    byte2 := make([]byte, 2)

    // Little vs Big endian?
    byte2[0] = uint8(bit16 >> 8)    // get 1st byte
    byte2[1] = uint8(bit16 & 0xFF)  // get 2nd byte
    fmt.Printf("bit16 = %x\n", bit16)
    fmt.Printf("byte2 = %x\n", byte2)
}

package main

import (
    "fmt"
    "flag"
)

func main() {
    wordPtr := flag.String("word", "foo", "a string")
    numPtr := flag.Int("num", 42, "an int")
    boolPtr := flag.Bool("fork", false, "a bool")

    var strVar string
    flag.StringVar(&strVar, "strvar", "bar", "a string var")

    flag.Parse()

    fmt.Println("word:", *wordPtr)
    fmt.Println("num:", *numPtr)
    fmt.Println("fork:", *boolPtr)
    fmt.Println("strvar:", strVar)
    fmt.Println("tail:", flag.Args()) // trailing positional arg
}

package main

import(
    "log"
    "github.com/iskhanba/misc_codes/golang/packages/pkgA"
    "./relative" // relative path import - BAD practice
)

func main() {

    log.Println("Top level main")
    pkgA.PackageA()

    // this is from hello.go
    // you don't need to import, BUT need to compile together,
    // if not, main.go can't find hellothere()
    // $ go run main.go hello.go
    hellothere()

    // relative path import
    // BAD practice, use for quick testing only
    relative.RelativePath()
}

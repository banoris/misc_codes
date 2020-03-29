package main

import(
    "log"
    "github.com/iskhanba/misc_codes/golang/packages/pkgA"
)

// can we have two main package inside a single repo?
// YES, no prob!
// NOTE that for `go run` to work, you need `package main` + func main() {...}
//   Something like python __main__ or C main() function
// $ go run main2.go hello.go
func main() {

    log.Println("Top level main")
    pkgA.PackageA()

    // this is from hello.go
    // you don't need to import, BUT need to compile together,
    // if not, main2.go can't find hellothere2()
    // $ go run main2.go hello.go
    hellothere2()
}

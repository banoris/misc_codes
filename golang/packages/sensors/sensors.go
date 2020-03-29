package main

// Go search path starts from $GOPATH/src/...
// so if the path has github.com/ than you need to include it as well
import(
    "log"
    "github.com/iskhanba/misc_codes/golang/packages/pkgA"
)

func main() {
    log.Println("sensor.go main")
    pkgA.PackageA()
}

package main

import (
    "fmt"
    "os"
    "os/signal"
    "syscall"
)

func main() {

    sigs := make(chan os.Signal, 1)
    done := make(chan bool, 1)

    signal.Notify(sigs, syscall.SIGINT, syscall.SIGTERM)

    go func() {
        //sig := <-sigs
        fmt.Println()
        //fmt.Println(sig)
        done <- true
    }()

    fmt.Println("awaiting signal")

    // How does go runtime determine this is not deadlock?
    // If you comment signal.Notify, then it complain deadlock
    <-done
    fmt.Println("exiting")
}

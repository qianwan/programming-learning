package main

import (
    "fmt"
    "time"
    "runtime"
)

func Count(ch chan int) {
    fmt.Println("Counting")
    ch <- 1
}

func main() {
    chs := make([]chan int, 10)
    for i := 0; i < 10; i++ {
        chs[i] = make(chan int)
        go Count(chs[i])
    }

    for _, ch := range chs {
        <-ch
    }

    fmt.Println(runtime.NumCPU())
    time.Sleep(1e9)
}
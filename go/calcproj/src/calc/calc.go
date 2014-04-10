package main

import (
    "os"
    "fmt"
    "simplemath"
    "strconv"
)

var Usage = func() {
    fmt.Println("USAGE: calc command [arguments] ...")
    fmt.Println("\nThe commands are:\n\tadd\tAddition of two values.\n\tsqrt\tSquare root of a non-negative value.")
}

func main() {
    args := os.Args
    if len(args) < 2 {
        Usage()
        return
    }
    switch args[1] {
    case "add":
        if len(args) != 4 {
            fmt.Println("Usage: calc add <integer1><integer2>")
            return
        }
        v1, err1 := strconv.Atoi(args[2])
        v2, err2 := strconv.Atoi(args[3])
        if err1 != nil || err2 != nil {
            fmt.Println("Usage: calc add <integer1><integer2>")
            return
        }
        ret := simplemath.Add(v1, v2)
        fmt.Println("Result: ", ret)
    default:
        Usage()
    }
}
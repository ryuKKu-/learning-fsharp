module LargestSeriesProduct

open NUnit.Framework
open FsUnit

let inline charToInt c = int c - int '0'

let largestProduct (input: string) (seriesLength: int) : int option = 
    if seriesLength = -1 || input.Length < seriesLength then
        None
    else
        let mutable result = 0
        let array = input |> Seq.map charToInt |> Seq.toArray
        for i in 0 .. array.Length - 1 - seriesLength do
            let x = array[i..seriesLength] |> Array.reduce (fun a b -> a * b)
            if x > result then
                result <- x
        Some result


[<Test>]
let ``Product of first and second prime`` () =
    largestProduct "29" 2 |> should equal (Some 18)
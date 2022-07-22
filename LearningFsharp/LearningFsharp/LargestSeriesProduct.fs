module LargestSeriesProduct

open NUnit.Framework
open FsUnit
open System.Text.RegularExpressions

let inline charToInt c = int c - int '0'

let largestProduct (input: string) (seriesLength: int) : int option =         
    let array = input |> Seq.map charToInt |> Seq.toArray
    if seriesLength = -1 || input.Length < seriesLength || array |> Array.exists (fun x -> x > 9) then 
        None
    else
        let mutable result = 0
        if seriesLength = 0 then
            Some 1
        else
        if seriesLength >= array.Length then 
            let x = array |> Array.reduce (fun a b -> a * b)
            if x > result then
                result <- x
        else
            for i in 0 .. array.Length - seriesLength do
                let z = seriesLength + i - 1           
                let x = array[i..z] |> Array.reduce (fun a b -> a * b)
                if x > result then
                    result <- x
        Some result


let largestProduct2 (input: string) (seriesLength: int) : int option = 
    match input, seriesLength with 
        | _, 0 -> Some 1
        | _, -1 -> None
        | "", l  when l > 0 -> None
        | i, l when i.Length < l -> None
        | i, _ when Regex.IsMatch(i, "\D") -> None
        | _, _ -> 
            input 
                |> Seq.map (string >> int)
                |> Seq.windowed seriesLength
                |> Seq.map (Array.reduce (*))
                |> (Seq.max >> Some)

[<Test>]
let ``Product of first and second prime`` () =
    largestProduct2 "29" 2 |> should equal (Some 18)

[<Test>]
let ``Can find the largest product of 2 with numbers in order`` () =
    largestProduct2 "0123456789" 2 |> should equal (Some 72)

[<Test>]
let ``Can find the largest product of 2`` () =
    largestProduct2 "576802143" 2 |> should equal (Some 48)

[<Test>]
let ``Can find the largest product of 3 with numbers in order`` () =
    largestProduct2 "0123456789" 3 |> should equal (Some 504)

[<Test>]
let ``Can get the largest product of a big number`` () =
    largestProduct2 "73167176531330624919225119674426574742355349194934" 6 |> should equal (Some 23520)

[<Test>]
let ``Reports zero if the only digits are zero`` () =
    largestProduct2 "0000" 2 |> should equal (Some 0)

[<Test>]
let ``Rejects span longer than string length`` () =
    let digits = "123"
    let span = 4
    largestProduct2 digits span |> should equal None

[<Test>]
let ``Rejects empty string and nonzero span`` () =
    let digits = ""
    let span = 1
    largestProduct2 digits span |> should equal None

[<Test>]
let ``Rejects invalid character in digits`` () =
    let digits = "1234a5"
    let span = 2
    largestProduct2 digits span |> should equal None

[<Test>]
let ``Rejects negative span`` () =
    let digits = "12345"
    let span = -1
    largestProduct2 digits span |> should equal None

[<Test>]
let ``Reports 1 for empty string and empty product (0 span)`` () =
    let digits = ""
    let span = 0
    largestProduct2 digits span |> should equal (Some 1)


[<Test>]
let ``Reports 1 for nonempty string and empty product (0 span)`` () =
    let digits = "123"
    let span = 0
    largestProduct2 digits span |> should equal (Some 1)
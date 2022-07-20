module PrimeFactors

open NUnit.Framework
open FsUnit

let factors (number: int64) = 
    let rec loop (acc: int64 list) (number: int64) (divider: int64): int64 list =
        match number with 
            | 1L -> acc
            | _ when number % divider = 0 -> loop (divider::acc) (number/divider) divider
            | _ when number % divider <> 0 -> loop acc number (divider+1L)
            | _ -> failwith "invalid input"
    loop [] number 2L |> List.map int32 |> List.rev


let factors2 (number: int64) = 
    let mutable remainder = number
    let mutable divider = 2L
    let mutable result = []

    while remainder <> 1 do
        if (remainder % divider) = 0 then
            remainder <- remainder / divider
            result <- divider::result
        else
            divider <- divider + 1L
    result |> List.map int32 |> List.rev
    

[<Test>]
let ``Product of first and second prime`` () =
    factors2 6L |> should equal [2; 3]
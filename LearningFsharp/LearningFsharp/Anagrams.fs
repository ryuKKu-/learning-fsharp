module Anagrams

open NUnit.Framework
open FsUnit
open System

let (|LowerWord|) (word: string) = word.ToLower()

let findAnagrams (sources: string list) (LowerWord target) = 
    let sort (LowerWord word) = word |> (Seq.sort >> String.Concat)

    sources 
        |> List.filter (fun (LowerWord x) -> x <> target)
        |> List.filter (fun (LowerWord x) -> sort x = sort target)

[<Test>]
let ``No matches`` () =
    let candidates = ["hello"; "world"; "zombies"; "pants"]
    findAnagrams candidates "diaper" |> should be Empty

[<Test>]
let ``Detects two anagrams`` () =
    let candidates = ["lemons"; "cherry"; "melons"]
    findAnagrams candidates "solemn" |> should equal ["lemons"; "melons"]

[<Test>]
let ``Words are not anagrams of themselves (case-insensitive)`` () =
    let candidates = ["BANANA"; "Banana"; "banana"]
    findAnagrams candidates "BANANA" |> should be Empty
module TwoFer

open NUnit.Framework
open FsUnit

let twoFer input =
    input 
    |> Option.defaultValue "you"
    |> sprintf "One for %s, one for me."

[<TestCase("Alice", "One for Alice, one for me.")>]
[<TestCase("Bob", "One for Bob, one for me.")>]
[<TestCase(null, "One for you, one for me.")>]
let ``Should return log message`` (str: string, expected: string) =
    twoFer (Option.ofObj str) |> should equal expected
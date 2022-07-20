module LogLevels

open System.Text.RegularExpressions
open NUnit.Framework
open FsUnit

let (|Log|_|) input = 
    let m = Regex.Match(input, "(?:\[(\w*)\])(?::\s)(.*)")
    if m.Success then 
        Some (m.Groups.[1].Value.ToLower(), m.Groups.[2].Value.Trim()) 
    else 
        None

let message (logLine: string): string = 
    match logLine with 
        | Log (_, message) -> sprintf "%s" message
        | _ -> "invalid string"

let logLevel(logLine: string): string = 
    match logLine with 
        | Log (level, _) -> sprintf "%s" level
        | _ -> "invalid string"

let reformat(logLine: string): string = 
    match logLine with 
        | Log (level, message) -> sprintf "%s (%s)" message level
        | _ -> "invalid string"


[<TestCase("[ERROR]: Invalid operation", "Invalid operation")>]
[<TestCase("[WARNING]:  Disk almost full\r\n", "Disk almost full")>]
let ``Should return log message`` (str: string, expected: string) =
    message str |> should equal expected

[<TestCase("[ERROR]: Invalid operation", "error")>]
[<TestCase("[WARNING]:  Disk almost full\r\n", "warning")>]
let ``Should return log level`` (str: string, expected: string) =
    logLevel str |> should equal expected

[<TestCase("[INFO]: Operation completed", "Operation completed (info)")>]
let ``Should reformat log`` (str: string, expected: string) =
    reformat str |> should equal expected
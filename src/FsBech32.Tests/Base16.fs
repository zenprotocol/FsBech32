module FsBech32.Tests.Base16

open NUnit.Framework
open FsUnit
open FsBech32

[<Test>]
let ``when decoding a non hex begin string should return None``() =
        Base16.decode "aataa" 
        |> Option.bind Base16.tryEncode
        |> should be Null
        
[<Test>]
let ``when decoding a non hex end string should return None``() =
        Base16.decode "aaaaat" 
        |> Option.bind Base16.tryEncode
        |> should be Null
[<Test>]        
let ``when decoding a  hex string should return Some``() =
        
        Base16.decode "aa" 
        |> Option.bind Base16.tryEncode
        |> should equal (Some "aa")
        
        

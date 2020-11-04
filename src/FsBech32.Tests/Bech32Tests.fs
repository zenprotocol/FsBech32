module FsBech32.Tests.Bech32Tests

open NUnit.Framework
open FsUnit
open FsBech32

let validBech32 = 
    [
        "A12UEL5L";
        "a12uel5l";
        "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs";
        "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw";
        "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j";
        "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w";
        "?1ezyfcl" ;
    ]
    
let invalidBech32 = 
    [
        " 1nwldj5";
        "\x7f1axkwrx";
        "\x801eym55h";
        "an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx";
        "pzry9x0s0muk";
        "1pzry9x0s0muk";
        "x1b4n0q5v";
        "li1dgmt3";
        "de1lg7wt\xFF";
        "A1G7SGD8";
        "10a06t8";
        "1qzzfhee";
    ]    
 
[<Test>]
let ``valid bech32 strings``() =
    List.iter (fun bech32 -> 
        match Bech32.decode bech32 with
        | Some (hrp, data) ->
             Bech32.encode hrp data |> should equal (bech32.ToLower())
        | None -> failwithf "invalid bech32 %s" bech32) validBech32             

[<Test>]
let ``invalid bech32 strings``() =
    List.iter (fun bech32 ->
        Bech32.decode bech32 |> should equal None) invalidBech32        
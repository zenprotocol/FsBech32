module FsBech32.Tests.SegWitAddressTests

open NUnit.Framework
open FsUnit
open FsBech32

let validAddresses = 
    [
        "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4","751e76e8199196d454941c45d1b3a323f1433bd6";
        "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7","1863143c14c5166804bd19203356da136c985678cd4d27a1b8c6329604903262";
        "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7k7grplx","751e76e8199196d454941c45d1b3a323f1433bd6751e76e8199196d454941c45d1b3a323f1433bd6";
        "BC1SW50QA3JX3S", "751e";
        "bc1zw508d6qejxtdg4y5r3zarvaryvg6kdaj","751e76e8199196d454941c45d1b3a323";
        "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy","000000c4a5cad46221b2a187905e5266362b99d5e91c6ce24d165dab93e86433"
    ]
    
let invalidAddresses = 
    [        
        "tc1qw508d6qejxtdg4y5r3zarvary0c5xw7kg3g4ty";
        "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t5";
        "BC13W508D6QEJXTDG4Y5R3ZARVARY0C5XW7KN40WF2";      
        "bc1rw5uspcuh";
        "bc10w508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kw5rljs90";
        "BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P";
        "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sL5k7";
        "bc1zw508d6qejxtdg4y5r3zarvaryvqyzf3du";
        "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3pjxtptv";
        "bc1gmk9yu";
    ]         

[<Test>]
let ``valid segwit addresses``() = 
    List.iter (fun (bech32, data) -> 
        match SegWitAddress.decode bech32 with 
        | None -> failwithf "not valid bech32 %s" bech32
        | Some (hrp, version, data') ->
            Some data' |> should equal (Base16.decode data)
            
            SegWitAddress.encode hrp version data' |> should equal (bech32.ToLower())                  
    ) validAddresses
    
[<Test>]
let ``invalid addresses``() = 
    List.iter (fun bech32 ->
        SegWitAddress.decode bech32 |> should equal None) invalidAddresses
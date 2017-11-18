module Base32

open FsBech32.Helper

let charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

let internal charsetRev = [|
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    15uy; 255uy; 10uy; 17uy; 21uy; 20uy; 26uy; 30uy;  7uy;  5uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 29uy; 255uy; 24uy; 13uy; 25uy;  9uy;  8uy; 23uy; 255uy; 18uy; 22uy; 31uy; 27uy; 19uy; 255uy;
     1uy;  0uy;  3uy; 16uy; 11uy; 28uy; 12uy; 14uy;  6uy;  4uy;  2uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 29uy; 255uy; 24uy; 13uy; 25uy;  9uy;  8uy; 23uy; 255uy; 18uy; 22uy; 31uy; 27uy; 19uy; 255uy;
     1uy;  0uy;  3uy; 16uy; 11uy; 28uy; 12uy; 14uy;  6uy;  4uy;  2uy; 255uy; 255uy; 255uy; 255uy; 255uy
|]

let encode (data:array<byte>) =
    match convertBits data 8 5 true with
        | None -> failwith "failed to converts bits"
        | Some data ->         
            Array.map (fun b -> charset.[int b]) data
            |> System.String    
    
let decode (base32:string) =     
    let data = 
        Seq.map (fun c -> byte (charsetRev.[int c])) base32
        |> Array.ofSeq
    
    match convertBits data 5 8 false with
    | None -> failwith "failed to converts bits"
    | Some data -> Some data
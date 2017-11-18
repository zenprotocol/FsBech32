module Base16

open FsBech32.Helper

let charset = "0123456789abcdef"

let internal charsetRev = [|
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    0uy;   1uy;   2uy;   3uy;   4uy;   5uy;   6uy;   7uy;   8uy;   9uy;   255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 10uy;  11uy;  12uy;  13uy;  14uy;  15uy;  255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 10uy;  11uy;  12uy;  13uy;  14uy;  15uy;  255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy
|]

let encode (data:array<byte>) = 
    match convertBits data 8 4 false with
    | None -> failwith "failed to converts bits"
    | Some data ->         
        Array.map (fun b -> charset.[int b]) data
        |> System.String 
        
let decode (base16:string) =
    // TODO: checks that all in range
    // TODO: checks that all same case
    // TODO: convert to lower case
 
    let data = 
        Seq.map (fun (c:char) -> byte (charsetRev.[int c])) base16
        |> Array.ofSeq
    
    match convertBits data 4 8 false with
    | None -> failwith "failed to converts bits"
    | Some data -> Some data
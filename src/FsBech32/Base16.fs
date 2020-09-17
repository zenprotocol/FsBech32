module FsBech32.Base16

open FsBech32.Helper

let [<Literal>] Charset = "0123456789abcdef"
    
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

let tryEncode (data:array<byte>): string option =
    let inner data =
        data
        |> Array.map (fun b -> Charset.[int b]) 
        |> System.String
    
    convertBits data 8 4 false
    |> Option.map inner

let encode (data:array<byte>) =
    tryEncode data
    |> Option.defaultWith (fun () -> failwith "failed to converts bits")
    
let decode: string -> option<byte[]> = 
    Seq.map (fun c -> byte (charsetRev.[int c]))
    >> Array.ofSeq
    >> (fun data -> convertBits data 4 8 false)

module FsBech32.Base16

open FsBech32.Helper
open FSharpx.Option

let [<Literal>] Charset = "0123456789abcdef"
    
    
let internal charsetRev = [|
    None;     None;      None;      None;      None;      None;      None;      None;     None;     None;     None;     None;     None;     None;     None;     None;
    None;     None;      None;      None;      None;      None;      None;      None;     None;     None;     None;     None;     None;     None;     None;     None;
    None;     None;      None;      None;      None;      None;      None;      None;     None;     None;     None;     None;     None;     None;     None;     None;
    Some 0uy; Some 1uy;  Some  2uy; Some 3uy;  Some 4uy;  Some 5uy;  Some 6uy;  Some 7uy; Some 8uy; Some 9uy; None;     None;     None;     None;     None;     None;
    None;     Some 10uy; Some 11uy; Some 12uy; Some 13uy; Some 14uy; Some 15uy; None;     None;     None;     None;     None;     None;     None;     None;     None;
    None;     None;      None;      None;      None;      None;      None;      None;     None;     None;     None;     None;     None;     None;     None;     None;
    None;     Some 10uy; Some 11uy; Some 12uy; Some 13uy; Some 14uy; Some 15uy; None;     None;     None;     None;     None;     None;     None;     None;     None;
    None;     None;      None;      None;      None;      None;      None;      None;     None;     None;     None;     None;     None;     None;     None;     None;
|]

let tryEncode (data:byte[]): string option =
    let inner data =
        data
        |> Array.map (fun b -> Charset.[int b]) 
        |> System.String
    
    convertBits data 8 4 false
    |> Option.map inner

let encode (data:byte[]) =
    tryEncode data
    |> Option.defaultWith (fun () -> failwith "failed to converts bits")
    
let decode: string -> option<byte[]> =
    Seq.toList
    >> FSharpx.Option.mapM (fun c -> charsetRev.[int c])
    >> Option.map Array.ofList
    >> Option.bind (fun data -> convertBits data 4 8 false)

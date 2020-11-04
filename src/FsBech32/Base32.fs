module FsBech32.Base32

open FsBech32.Helper

let Charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

let internal charsetRev = [|
    None;      None;      None;      None;      None;      None;      None;      None;      None;      None;     None;      None;      None;      None;      None;      None;
    None;      None;      None;      None;      None;      None;      None;      None;      None;      None;     None;      None;      None;      None;      None;      None;
    None;      None;      None;      None;      None;      None;      None;      None;      None;      None;     None;      None;      None;      None;      None;      None;
    Some 15uy; None;      Some 10uy; Some 17uy; Some 21uy; Some 20uy; Some 26uy; Some 30uy; Some 7uy;  Some 5uy; None;      None;      None;      None;      None;      None;
    None;      Some 29uy; None;      Some 24uy; Some 13uy; Some 25uy; Some 9uy;  Some 8uy;  Some 23uy; None;     Some 18uy; Some 22uy; Some 31uy; Some 27uy; Some 19uy; None;
    Some 1uy;  Some 0uy;  Some 3uy;  Some 16uy; Some 11uy; Some 28uy; Some 12uy; Some 14uy; Some 6uy;  Some 4uy; Some 2uy;  None;      None;      None;      None;      None;
    None;      Some 29uy; None;      Some 24uy; Some 13uy; Some 25uy; Some 9uy;  Some 8uy;  Some 23uy; None;     Some 18uy; Some 22uy; Some 31uy; Some 27uy; Some 19uy; None;
    Some 1uy;  Some 0uy;  Some 3uy;  Some 16uy; Some 11uy; Some 28uy; Some 12uy; Some 14uy; Some 6uy;  Some 4uy; Some 2uy;  None;      None;      None;      None;      None
|]

/// TODO: Base 32 is broken and needs a fix
//
//let tryEncode (data:byte[]): string option =
//    let inner data =
//        data
//        |> Array.map (fun b -> Charset.[int b]) 
//        |> System.String
//            
//    convertBits data 8 5 true
//    |> Option.map inner
//
//let encode (data:byte[]) =
//    tryEncode data
//    |> Option.defaultWith (fun () -> failwith "failed to converts bits")
//    
//let decode : string -> option<byte[]> =    
//    Seq.toList
//    >> FSharpx.Option.mapM (fun c -> (charsetRev.[int c]))
//    >> Option.map Array.ofList
//    >> Option.bind (fun data -> convertBits data 5 8 false)
//    

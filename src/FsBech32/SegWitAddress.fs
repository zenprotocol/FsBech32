module FsBech32.SegWitAddress

let decode addr : option<string * byte * array<byte> > =
    match Bech32.decode addr with
    | None -> None
    | Some (hrp, data) ->
        if (hrp <> "tb" && hrp <> "bc") || data.Length = 0 then
            None
        else
            let bytes = Bech32.fromWords data.[1..]
            match bytes with
            | None -> None
            | Some decoded ->
                let decodedLength = decoded.Length
                if decodedLength < 2 || decodedLength > 40 then 
                    None 
                else                                    
                    if data.[0] > 16uy then 
                        None
                    else
                        if data.[0] = 0uy && decodedLength <> 20 && decodedLength <> 32 then 
                            None
                        else 
                            Some (hrp, data.[0], decoded)


let tryEncode hrp witver witprog : string option =
    let inner words =
        Bech32.encode hrp (Array.append [|witver|] words)
    
    Bech32.tryToWords witprog
    |> Option.map inner
    
let encode hrp witver witprog : string =
    let words = Bech32.toWords witprog        
    Bech32.encode hrp (Array.append [|witver|] words)        

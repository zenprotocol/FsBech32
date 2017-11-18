module Bech32

open FsBech32.Helper

let private bech32Polymod (values:array<byte>) : uint32 =
    let generator = [| 0x3b6a57b2ul; 0x26508e6dul; 0x1ea119faul; 0x3d4233ddul; 0x2a1462b3ul |]
    let mutable chk = 1ul
    for v in values do
        let top = chk >>> 25
        chk <- ( (chk &&& 0x1fffffful) <<< 5 ) ^^^ (uint32 v)
        for i in 0 .. 4 do
            if ((top >>> i) &&& 1ul) <> 0ul
            then chk <- chk ^^^ generator.[i]
            else chk <- chk ^^^ 0ul
    chk

let private bech32HRPExpand (hrp:string) : array<byte> =
    Array.concat [ 
        [| for c in hrp do yield (byte c) >>> 5 |]
        [| 0uy |]
        [| for c in hrp do yield (byte c) &&& 31uy |]
    ]

let private bech32VerifyChecksum hrp data : bool =
    let values = Array.append (bech32HRPExpand hrp) data
    let checksum = bech32Polymod values        
    
    checksum = 1ul

let private bech32CreateChecksum hrp data : array<byte> =
    let values = Array.append (bech32HRPExpand hrp) data
    let polymod = bech32Polymod (Array.append values [| 0uy;0uy;0uy;0uy;0uy;0uy |]) ^^^ 1ul
    [| for i in 0..5 do yield byte (polymod >>> (5 * (5 - i)) &&& 31ul)  |]

let private bech32Encode hrp data : string =
    let combined = Array.append data (bech32CreateChecksum hrp data)
    hrp + "1" + (Array.map (fun c -> Base32.charset.[int c]) combined |> System.String)

let private bech32Decode bech : option< string * array<byte> > =
    try 
        let inRange = String.forall (fun c -> int c >= 33 && int c <= 126) bech
        let sameCase = bech.ToUpper() = bech || bech.ToLower() = bech
        let lengthOk = bech.Length <= 90
        if not (inRange && sameCase && lengthOk) then 
            None 
        else            
            let pos = bech.LastIndexOf "1"
            if pos < 1 || pos + 7 > bech.Length then 
                None 
            else
                let in_charset = 
                    bech.[(pos+1)..] |> String.forall (fun c -> Seq.contains c Base32.charset)
                if not in_charset then 
                    None 
                else
                    let hrp = bech.[..(pos-1)]
                    let data = [| for c in bech.[(pos+1)..] do yield byte (Base32.charsetRev.[int c]) |]
                    if not (bech32VerifyChecksum hrp data) then                                                
                        None 
                    else
                        Some (hrp, Array.take (data.Length - 6) data)
    with 
        | _ -> None

let decode addr : option<string * byte * array<byte>> =
    match bech32Decode addr with
    | None -> None
    | Some (hrp, data) ->        
        let decoded = convertBits data.[1..] 5 8 false
        match decoded with
        | None -> None
        | Some decoded ->            
            Some (hrp, data.[0], decoded)

let encode hrp version data : string =
    match convertBits data 8 5 true with
    | None -> failwith "converting bits failed" // This shouldn't happen, therefore throwing an exception
    | Some data ->
        bech32Encode hrp (Array.append [|version|] data)        
module FsBech32.Bech32

open FsBech32.Helper

let private polymod (values:byte[]) : uint32 =
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

let private expandHrp (hrp:string) : byte[] =
    Array.concat [ 
        [| for c in hrp do yield (byte c) >>> 5 |]
        [| 0uy |]
        [| for c in hrp do yield (byte c) &&& 31uy |]
    ]

let private verifyChecksum hrp data : bool =
    let values = Array.append (expandHrp hrp) data
    let checksum = polymod values        
    
    checksum = 1ul

let private createChecksum hrp data : array<byte> =
    let values = Array.append (expandHrp hrp) data
    let polymod = polymod (Array.append values [| 0uy;0uy;0uy;0uy;0uy;0uy |]) ^^^ 1ul
    [| for i in 0..5 do yield byte (polymod >>> (5 * (5 - i)) &&& 31ul)  |]

let encode hrp data : string =
    let combined = Array.append data (createChecksum hrp data)
    hrp + "1" + (Array.map (fun c -> Base32.Charset.[int c]) combined |> System.String)

let decode bech : option< string * array<byte> > =
    try 
        let inRange = String.forall (fun c -> int c >= 33 && int c <= 126) bech
        let sameCase = bech.ToUpper() = bech || bech.ToLower() = bech
        let lengthOk = bech.Length <= 90
        
        if not (inRange && sameCase && lengthOk) then 
            None 
        else            
            let pos = bech.LastIndexOf "1"
            let bech = bech.ToLower()
            if pos < 1 || pos + 7 > bech.Length then 
                None 
            else
                let words = bech.[(pos+1)..]           
                let in_charset = 
                    words |> String.forall (fun c -> Seq.contains c Base32.Charset)
                    
                let longEnough = words.Length >= 6                
                                                        
                if not (longEnough && in_charset) then 
                    None 
                else
                    let hrp = bech.[..(pos-1)]
                    let data =
                        [ for w in words do yield Base32.charsetRev.[int w] ]
                        |> FSharpx.Option.sequence
                        |> Option.map List.toArray

                    match data with     
                    | Some data ->
                        if not (verifyChecksum hrp data) then                                                
                            None 
                        else                         
                            Some (hrp, Array.take (data.Length - 6) data)
                    | None ->
                        None 
    with 
        | _ -> None


let tryToWords bytes : array<byte> option =
    convertBits bytes 8 5 true

let toWords bytes = 
    tryToWords bytes
    |> Option.defaultWith (fun () -> failwith "toWords failed")
    
let fromWords words = 
    convertBits words 5 8 false   

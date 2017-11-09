module Bech32

let charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

let bech32Polymod (values:array<int>) : int =
    let generator = [| 0x3b6a57b2; 0x26508e6d; 0x1ea119fa; 0x3d4233dd; 0x2a1462b3 |]
    let mutable chk = 1
    for v in values do
        let top = chk >>> 25
        chk <- ( (chk &&& 0x1ffffff) <<< 5 ) ^^^ v
        for i in 0 .. 4 do
            if ((top >>> i) &&& 1) <> 0
            then chk <- chk ^^^ generator.[i]
            else chk <- chk ^^^ 0
    chk

let bech32HRPExpand (hrp:string) : array<int> =
    Array.concat [ 
        [| for c in hrp do yield (int c) >>> 5 |]
        [| 0 |]
        [| for c in hrp do yield (int c) &&& 31 |]
    ]

let bech32VerifyChecksum hrp data : bool =
    let values = Array.append (bech32HRPExpand hrp) data
    let checksum = bech32Polymod values
    checksum = 1

let bech32CreateChecksum hrp data : array<int> =
    let values = Array.append (bech32HRPExpand hrp) data
    let polymod = bech32Polymod (Array.append values [| 0;0;0;0;0;0 |]) ^^^ 1
    [| for i in 0..5 do yield polymod >>> (5 * (5 - i)) ^^^ 31  |]

let bech32Encode hrp data : string =
    let combined = Array.append data (bech32CreateChecksum hrp data)
    hrp + "1" + (Array.map char combined |> System.String)

let bech32Decode bech : option< string * array<int> > =
    try 
        let inRange = String.forall (fun c -> int c >= 33 && int c <= 126) bech
        let sameCase = bech.ToUpper() = bech || bech.ToLower() = bech
        let lengthOk = bech.Length <= 90
        if not (inRange && sameCase && lengthOk) then None else
        let bech = bech . ToLower()
        let pos = bech . LastIndexOf "1"
        if pos < 1 || pos + 7 > bech.Length then None else
        let in_charset = 
            bech.[(pos+1)..] |> String.forall (fun c -> Seq.contains c charset)
        if not in_charset then None else
        let hrp = bech.[..pos]
        let data = [| for c in bech.[(pos+1)..] do yield charset . IndexOf c |]
        if not (bech32VerifyChecksum hrp data) then None else
        Some (hrp, Array.take (data.Length - 6) data)
    with 
        | _ -> None


let convertBits (data: array<int>) (fromBits:int) (toBits:int) (maybePad:bool) 
    : option< array<int> > =
    let mutable acc, bits, ret = 0, 0, [||]
    let maxv = (1 <<< toBits) - 1
    let maxAcc = (1 <<< (fromBits + toBits - 1)) - 1
    
    let mutable stop = false
    for value in data do
        if stop then ()
        if value < 0 || (value >>> fromBits) <> 0 
        then stop <- true
        else
            acc <- ((acc <<< fromBits) ||| value) &&& maxAcc
            bits <- bits + fromBits
            while bits >= toBits do
                bits <- bits - toBits
                ret <- Array.append ret 
                                    [| (acc >>> bits) &&& maxv |]
    
    if stop then None else
    if maybePad then 
        if bits <> 0 
        then Some <| Array.append ret [| (acc <<< (toBits - bits)) &&& maxv |]
        else Some ret
    elif bits >= fromBits || ((acc <<< (toBits - bits)) &&& maxv) <> 0 then
        None
    else 
        Some ret


let decode hrp addr : option< int * array<int> > =
    match bech32Decode addr with
    | None -> None
    | Some (hrp', data) ->
        if hrp' <> hrp then None else
        let decoded = convertBits data.[1..] 5 8 false
        match decoded with
        | None -> None
        | Some decoded ->
            let decodedLength = decoded.Length
            if decodedLength < 2 || decodedLength > 40 then None else
            if data.[0] > 16 
            then None else
            if data.[0] = 0 && decodedLength <> 20 && decodedLength <> 32 
            then None
            else Some (data.[0], decoded)

let encode hrp witver witprog : option<string>=
    match convertBits witprog 8 5 true with
    | None -> None
    | Some data ->
        let ret = bech32Encode hrp (Array.append [|witver|] data)
        match decode hrp ret with
        | None -> None
        | Some _ -> Some ret
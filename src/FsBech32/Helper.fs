module FsBech32.Helper

type private Char = System.Char

let rec private foldOpt (f: 'state -> 'a -> option<'state>)
                (state:'state)
                (xs: seq<'a>)
                : option<'state> =
    if Seq.isEmpty xs then Some state else
    match f state (Seq.head xs) with
    | None -> None
    | Some nextState -> foldOpt f nextState (Seq.tail xs)

// checks that all chars are in range, in the same case, and converts to lower case
let validate: string -> option<string> =
    let aux (case:option<bool>, state:seq<char>) (c:char)
            :option<option<bool> * seq<char>> =
        begin if Char.IsDigit c 
            then case, Some c
        elif Char.IsUpper c && case <> Some false
            then Some true, Some (Char.ToLower c)
        elif Char.IsLower c && case <> Some true
            then Some false, Some c
        else case, None end
        |> function
           | _, None -> None
           | case, Some c -> Some (case, seq {yield c; yield! state})
    
    foldOpt aux (None, Seq.empty)
    >> Option.map (snd >> Seq.rev >> Array.ofSeq >> System.String)

let convertBits (data: byte[]) (fromBits:int) (toBits:int) (maybePad:bool) : option<byte[]> =
           
    let mutable value = 0ul
    let mutable bits = 0
    let mutable outputRev = []
    
    let maxv = ((1ul) <<< toBits) - 1ul;
    
    for b in data do                
        value <- (value <<< fromBits) ||| (uint32 b)
        bits  <- bits + fromBits
        
        while bits >= toBits do
            bits <- bits - toBits;
            outputRev <- byte ((value >>> bits) &&& maxv) :: outputRev                                        
                        
    if maybePad then
        if bits <> 0 then
            outputRev <- byte ((value <<< (toBits - bits)) &&& maxv) :: outputRev
        
        List.rev outputRev |> List.toArray |> Some
                    
    elif ((value <<< (toBits - bits)) &&& maxv) <> 0ul || bits >= fromBits then
        None        
    else
        List.rev outputRev |> List.toArray |> Some
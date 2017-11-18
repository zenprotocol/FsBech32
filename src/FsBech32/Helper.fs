module FsBech32.Helper

let convertBits (data: array<byte>) (fromBits:int) (toBits:int) (maybePad:bool) : option< array<byte> > =
           
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
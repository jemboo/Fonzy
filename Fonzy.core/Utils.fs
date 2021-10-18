namespace global
open System.Collections.Generic
open Microsoft.FSharp.Core
open System
open System.Security.Cryptography
open System.Runtime.Serialization.Formatters.Binary
open System.IO
open System.Numerics


module ByteUtils =

    let bytesForObj (o:obj) =
        let bf = new BinaryFormatter()
        use ms = new MemoryStream()
        bf.Serialize(ms, o);
        let rv = ms.ToArray()
        ms.Dispose()
        rv

    let structHash (o:obj) =
        let md5 = MD5.Create();
        md5.ComputeHash(bytesForObj o)


    let trueBitCount32 (u32:uint) =
        let mutable tc = 0
        for i in 0 .. 31 do
            let qua = (u32 &&& (1u <<< i)) > 0u
            if qua then
                tc <- tc + 1
        tc

    let trueBitCount64 (u64:uint64) =
        let mutable tc = 0
        for i in 0 .. 63 do
            let qua = (u64 &&& (1UL <<< i)) > 0UL
            if qua then
                tc <- tc + 1
        tc


    let trueBitIndexes64 (u64:uint64) =
        seq {
                for i in 0 .. 63 do
                    if (u64 &&& (1UL <<< i)) > 0UL then
                        yield i
            }


    let stripeWrite (uBits:uint64[]) 
                    (intBits:int[]) 
                    (pos:int) = 
        let one = (1UL <<< pos)
        let proc dex =
            if (intBits.[dex] > 0) then
                uBits.[dex] <- 
                            uBits.[dex] ||| one
    
        for i=0 to (uBits.Length - 1) do
            proc i


    let stripeRead (uBits:uint64[]) 
                   (pos:int) = 
        let one = (1UL <<< pos)
        let proc dex v =
            if ((uBits.[dex] &&& one) > 0UL) then
                1
            else 0
        uBits |> Array.mapi (proc)


module GuidUtils = 

    let makeGuid (g1:uint64) (g2:uint64) (g3:uint64) (g4:uint64) =
        let pc0 = System.BitConverter.GetBytes(g1)
        let pc1 = System.BitConverter.GetBytes(g2)
        let pc2 = System.BitConverter.GetBytes(g3)
        let pc3 = System.BitConverter.GetBytes(g4)

        let woof = seq {pc0.[0]; pc0.[1]; pc0.[2]; pc0.[3]; 
                        pc1.[0]; pc1.[1]; pc1.[2]; pc1.[3];
                        pc2.[0]; pc2.[1]; pc2.[2]; pc2.[3];
                        pc3.[0]; pc3.[1]; pc3.[2]; pc3.[3]; } |> Seq.toArray
        new System.Guid(woof)

    let addGuids (g1:Guid) (g2:Guid) =
        let pcs1 = g1.ToByteArray()
        let pcs2 = g2.ToByteArray()
        let pcsS = Array.init 16 (fun i-> pcs1.[i] + pcs2.[i])
        new System.Guid(pcsS)

    let addGuidsO (g1:Guid option) (g2:Guid option) =
        match g1,g2 with
        | Some v1, Some v2 -> addGuids v1 v2
        | None, Some v2 -> v2
        | Some v1, None -> v1
        | None, None -> Guid.Empty

    let guidFromObjs (objs:seq<obj>) =
        let acc = Array.create 16 (byte 0)
        let folder (a:byte[]) (b:byte[]) =
            a |> Array.map2(fun a b -> a+b) b

        let md5 = MD5.Create();
        let laz = objs |> Seq.map(fun o -> md5.ComputeHash(ByteUtils.structHash o))
                       |> Seq.fold(fun a b -> folder a b) acc
        System.Guid(laz)

    //let guidFromObjList (objs:obj list) =
    //    let acc = Array.create 16 (byte 0)
    //    let folder (a:byte[]) (b:byte[]) =
    //        a |> Array.map2(fun a b -> a+b) b

    //    let md5 = MD5.Create();
    //    let laz = objs |> List.map(fun o -> md5.ComputeHash(ByteUtils.structHash o))
    //                   |> List.fold(fun a b -> folder a b) acc
    //    System.Guid(laz)

    let guidFromStringR (gstr:string) =
        let mutable gv = Guid.NewGuid()
        match Guid.TryParse(gstr, &gv) with
        | true -> gv |> Ok
        | false -> "not a guid: " + gstr |> Result.Error
        
    let guidFromStringO (gstr:string) =
        let mutable gv = Guid.NewGuid()
        match Guid.TryParse(gstr, &gv) with
        | true -> gv |> Some
        | false -> None


module ParseUtils =

    let MakeInt32 (str:string) =
        let mutable oot = 0
        let res = Int32.TryParse(str, &oot)
        if res then
            oot |> Ok
        else
            sprintf "Not an int: %s" str |> Error

    let MakeFloat (str:string) =
        let mutable oot = 0.0
        let res = Double.TryParse(str, &oot)
        if res then
            oot |> Ok
        else
            sprintf "Not a float: %s" str |> Error

    let StringToOneInt (str:string) =
        let pcs = str.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        if pcs.Length <> 1 then
            sprintf "1 param expected, not %d" pcs.Length |> Error
        else
            MakeInt32 pcs.[0]

    let StringToOneFloat (str:string) =
        let pcs = str.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        if pcs.Length <> 1 then
            sprintf "1 param expected, not %d" pcs.Length |> Error
        else
            MakeFloat pcs.[0]


module SeqUtils =

    let join<'T> (second:seq<'T>)
                 (first:seq<'T>) = 
        seq { yield! first; yield! second }


type CircularBuffer<'T> (deflt:'T, size:int) =
    let arr = Array.create size deflt
    let mutable headIndex = -1

    member x.Push value =
        if headIndex + 1 = size then
            headIndex <- 0
        else
            headIndex <- headIndex + 1
        arr.[headIndex] <- value

    member x.SetCurrent value =
        arr.[headIndex] <- value

    member x.LastNticks with get(nTicks) =
        if nTicks > size then
            failwith "Number of requested ticks exceeds number of ticks"
        elif headIndex >= nTicks - 1 then
            let startIndex = (headIndex - nTicks + 1)
            arr.[startIndex..(headIndex)]
        else
            let offset = size - (nTicks - headIndex - 1)
            let startArray = arr.[offset..]
            let endArray = arr.[0..headIndex]
            Array.append startArray endArray
    
    member x.GetTick(nTicksPrior) =
        if headIndex >= nTicksPrior then
            arr.[headIndex - nTicksPrior]
        else
            let offset = size - 1 - (headIndex - nTicksPrior)
            arr.[offset]

    member x.Current =
        x.GetTick(0)



module CollectionUtils =
    // Generates an n-dimenstional cartesian product, with n = LL.Length
    //let rec cart1 LL = 
    //    match LL with
    //    | [] -> Seq.singleton []
    //    | L::Ls -> seq {for x in L do for xs in cart1 Ls -> x::xs}
    let rec cartesian = function
    | ([],[]) -> []
    | (xs,[]) -> []
    | ([],ys) -> []
    | (x::xs, ys) -> (List.map(fun y -> x,y) ys) @ (cartesian (xs,ys))

    let listLoop<'T> (a:'T list) = 
        Seq.initInfinite (fun d -> a.[d % a.Length])
        
    let arrayLoop (count:int) (ofWhat:'a[]) =
        seq { for i in 0..(count-1) 
                    do yield ofWhat.[i%ofWhat.Length] }

    //returns the last n items of the list in the original order
    let rec last n xs =
      if List.length xs <= n then xs
      else last n xs.Tail

    //returns the first n items of the list in the original order,
    //or all the items if it's shorter than n
    let first n (xs:'a list) =
        let mn = min n xs.Length
        xs |> List.take mn

    // makes a sliding window up to width max, or less if there are
    // not enough elements
    let maxWindowed (max:int) (items: seq<'a>) =
        let mutable window = List.Empty
        let trim() =
            if window.Length = max then
               window |> first (max - 1)
            else    
               window
        seq {for item in items do
                 window <- trim()
                 window <- window |> List.append [item]
                 yield window |> List.rev}

    // returns an array of length chunkSz, which is made by converting vals to a
    // 2d array with chunkSz columns, and then summing over each column. 
    let wrapAndSumCols (chunkSz:int) (vals:seq<int>) =
        let addArrays (a:int[]) (b:int[]) =
            Array.init a.Length (fun dex -> a.[dex] + b.[dex])

        vals |> Seq.chunkBySize chunkSz
             |> Seq.toArray
             |> Array.reduce addArrays

// Splits the sourceArray into segments using segBounds
    let breakArrayIntoSegments (sourceArray : array<'a>) 
                               (segBounds : array<int>) =
        seq {1 .. (segBounds.Length - 1) }
        |> Seq.map(fun i -> sourceArray.[segBounds.[i - 1] .. (segBounds.[i] - 1)])
        |> Seq.toArray


    let listToTransitionTuples (ltt:'a list) =
        let rec yucko (last:'a) (tail:'a list) (tupes: ('a*'a) list) =
            match tail with
            | [] -> tupes
            | head::rest -> yucko head rest ((last,head)::tupes)
        match ltt with
        | [] -> []
        | head::tail -> yucko head tail [] |> List.rev


    let repeater f (items:'a[]) (count:int) =
        let tt = seq {for i=0 to (items.Length-1) 
                        do yield! Seq.replicate count (f items.[i]) }
        seq { while true do yield! tt }


    // Converts seq of key - value pairs to mutable Dictionary
    let tuplesToDict(src:seq<'a * 'b>) = 
       let dictionary = new Dictionary<'a,'b>()
       for (k,v) in src do
           dictionary.Add(k,v)
       dictionary

    // returns a list of the items that were added
    let addDictionary (dBase:Dictionary<'a,'b>) (dAdd:Dictionary<'a,'b>) =
        let mutable lstRet = []
        dAdd.Keys |> Seq.iter(fun k-> 
            if (not (dBase.ContainsKey(k))) then  
               dBase.Add(k, dAdd.[k])                       
               lstRet <- dAdd.[k]::lstRet)
        lstRet

    // returns a sequence of items that occur more than once 
    let itemsOccuringMoreThanOnce items =
        seq {
            let d = System.Collections.Generic.Dictionary()
            for i in items do
                match d.TryGetValue(i) with
                | false, _    -> d.[i] <- false         // first observance
                | true, false -> d.[i] <- true; yield i // second observance
                | true, true  -> ()                     // already seen at least twice
        }

         
    let mapSubset (m:Map<'a,'v>) (keys:seq<'a>) = 
        keys |> Seq.map(fun k-> k, (m.[k]))
             |> Map.ofSeq

    let join (m:Map<'a,'v>) (j:seq<'a*'w>) =
        seq { for kv in j do
                if (m.ContainsKey (fst kv)) then
                        yield (fst kv, (m.[fst kv], snd kv)) }

    let flatten (arr:'a[]) (iron:'a->'b[]) =
        arr |> Seq.map(fun a-> iron a |> Array.toSeq)
            |> Seq.concat


    let sortedUnique<'T when 'T:equality and 'T:comparison> (items:'T seq) =
        items |> Seq.distinct |> Seq.sort

    // get a seq of key-value pairs for easy iteration with for (k,v) in d do...
    let toKeyValuePairs (d:Dictionary<'a, 'b>) =
       seq {
           for kv in d do
               yield (kv.Key, kv.Value)
       }

    let histogram<'d,'r when 'r:comparison> (keymaker:'d->'r) 
                    (qua:seq<'d>) =
        qua
        |> Seq.fold (fun acc fv ->
                let kk = keymaker fv
                if Map.containsKey kk acc
                then Map.add kk (acc.[kk] + 1) acc
                else Map.add kk 1 acc
            ) Map.empty

    let histoTotalCount<'a> (bins:('a*int)[]) = 
        bins |> Array.sumBy(snd)



module SizeOpt =

    let toSparseFormat (dfVal:'T) (aa:'T[] when 'T:equality)  =
        seq { for i=0 to (aa.Length - 1) do
                if aa.[i] <> dfVal then yield (i, aa.[i])  }


    let fromSparseFormat (dfVal:'T) (arrayLen:int) 
                         (aa:seq<(int*'T)> when 'T:equality) =
        let aB = Array.create arrayLen dfVal
        aa |> Seq.iter(fun tup -> aB.[fst tup] <- snd tup )
        aB

    // compresses a time series by producing (index, value) tuples
    let toTransitionFormat (aa:seq<'T> when 'T:equality) =
        seq {
            let ee = aa.GetEnumerator()
            let mutable dex = 0
            let mutable lastVal = None
            while ee.MoveNext() do
                match lastVal with
                | Some v -> if v <> ee.Current then
                                yield (dex, ee.Current)
                | None -> yield (dex, ee.Current)
                lastVal <- Some ee.Current
                dex <- dex + 1
        }


    let fromTransitionFormat (aa:seq<(int*'T)> when 'T:equality) 
                             (dv:'T) (maxDex:int) =
        let aB = Array.create maxDex dv
        aa |> Seq.iter(fun tup -> aB.[fst tup] <- snd tup )
        let mutable curV = aB.[0]
        for i=0 to (maxDex - 1) do
            if aB.[i] = dv then
                aB.[i] <- curV
            else
                curV <- aB.[i]
        aB

    // returns an array of (index, value pairs) that express the 
    // differences between the first and second arrays sndA must be
    // at least as long as fstA
    let toDiffFormat (fstA:'T[] when 'T:equality)
                 (sndA:'T[] when 'T:equality) =
        seq {
            for dex = 0 to (fstA.Length - 1) do
                if fstA.[dex] <> sndA.[dex] then
                    yield (dex, sndA.[dex])
        }

    let fromDiffFormat (fstA:'T[] when 'T:equality)
                       (diffs: (int*'T)[]) =
        let retA = fstA |> Array.copy
        diffs |> Array.iter(fun tup -> retA.[fst tup] <- snd tup )
        retA



module ResultMap =

    let fromTuples (tupes:('a*'b)[]) =
        let map = tupes |> Map.ofSeq
        if (map.Count = tupes.Length) then
            map |> Ok
        else "key duplicates" |> Error


    let read (key:'a) (m:Map<'a,'b>) =
        if (m.ContainsKey key) then m.[key] |> Ok
        else (sprintf "key %A missing" key) |> Error


    let readType<'a> (key:string) (m:Map<string,obj>) : Result<'a, string> =
        match m.TryFind key with
        | Some value -> 
          try
            (value :?> 'a) |> Ok
          with
          | :? InvalidCastException ->
            let typeName = typeof<'a>.Name
            sprintf "value could not be cast to %s" typeName |> Error
    
        | None -> sprintf "key not found: %s" key|> Error


    let add (key:'a) (vl:'v) (m:Map<'a,'v>) =
            match m.TryFind key with
            | Some value -> sprintf "key already present: %A" key |> Error
            | None -> m |> Map.add key vl |> Ok


    let lookupKeyedInt<'a> (key:string) 
                           (cs:Map<string, string>) : Result<int, string> =
        match cs.TryFind key with
        | Some value -> 
          try
            (value |> int) |> Ok
          with
          | :? InvalidCastException ->
            sprintf "value: %s could not be cast to int" value |> Error

        | None -> sprintf "key not found: %s" key|> Error

    let lookupKeyedFloat<'a> (key:string) 
                             (cs:Map<string, string>) : Result<float, string> =
        match cs.TryFind key with
        | Some value -> 
          try
            (value |> float) |> Ok
          with
          | :? InvalidCastException ->
            sprintf "value: %s could not be cast to float" value |> Error

        | None -> sprintf "key not found: %s" key|> Error

    let lookupKeyedGuid<'a> (key:string) 
                            (cs:Map<string, string>) : Result<Guid, string> =
        match cs.TryFind key with
        | Some value -> 
          try
            Guid.Parse(value) |> Ok
          with
          | :? InvalidCastException ->
            sprintf "value: %s could not be cast to float" value |> Error

        | None -> sprintf "key not found: %s" key|> Error

    let lookupKeyedBool<'a> (key:string) 
                            (cs:Map<string, string>) : Result<bool, string> =
        match cs.TryFind key with
        | Some value -> 
          try
            (value |> bool.Parse) |> Ok
          with
          | :? InvalidCastException ->
            sprintf "value: %s could not be cast to bool" value |> Error

        | None -> sprintf "key not found: %s" key|> Error


    let procKeyedInt<'a> (key:string) (proc:int->Result<'a,string>) 
                         (cs:Map<string, string>) : Result<'a, string> =
        result {
            let! iv = lookupKeyedInt key cs
            return! proc iv
        }

    let procKeyedFloat<'a> (key:string) (proc:float->Result<'a,string>) 
                           (cs:Map<string, string>) : Result<'a, string> =
        result {
            let! iv = lookupKeyedFloat key cs
            return! proc iv
        }

    let procKeyedString<'a> (key:string) (proc:string->Result<'a,string>) 
                          (cs:Map<string, string>) : Result<'a, string> =
        result {
            let! cereal = read key cs
            return! proc cereal
        }

    let procKeyedGuid<'a> (key:string) (proc:Guid->Result<'a,string>) 
                          (cs:Map<string, string>) : Result<'a, string> =
        result {
            let! cereal = lookupKeyedGuid key cs
            return! proc cereal
        }



module StringUtils =

    let printIntArray (d:int[]) =
        let sb = new System.Text.StringBuilder()
        d |> Seq.map(fun i -> sb.Append(sprintf "%d%s" i Environment.NewLine))
          |> Seq.toArray
          |> ignore
        sb.ToString()

    let printArray (d:'a[]) (delimiter:string) =
        let sb = new System.Text.StringBuilder()
        d |> Seq.map(fun i -> sb.Append(sprintf "%A%s" i delimiter))
          |> Seq.toArray
          |> ignore
        sb.ToString()

    let printLinesOfArray (d:'a[]) =
        printArray d Environment.NewLine

    let printArrayAsTabDelimited (d:'a[]) =
        printArray d "\t"

    let printArrayf f (d:'a[]) (delimiter:string) =
        let sb = new System.Text.StringBuilder()
        d |> Seq.map(fun i -> sb.Append(sprintf "%A%s" (f i) delimiter))
          |> Seq.toArray
          |> ignore
        sb.ToString()

    let printLinesOfArrayf f (d:'a[]) =
        printArrayf f d Environment.NewLine

    let printTupes (d:seq<string*'A>) =
       let sb = new System.Text.StringBuilder()
       d |> Seq.map(fun i -> sb.Append(sprintf "%s=%A, " (fst i) (snd i)))
         |> Seq.toArray
         |> ignore
       sb.ToString()
       
    let optDto (v:'a option) (f:'a->'b) =
        match v with
        | Some av -> Some (f av)
        | None -> None



module FuncUtils = 
    let memoization f =
        // The dictionary is used to store values for every parameter that has been seen
        let cache = Dictionary<_,_>()
        fun c ->
            let exist, value = cache.TryGetValue (c)
            match exist with
            | true -> 
                // Return the cached result directly, no method call
                printfn "%O -> In cache" c
                value
            | _ -> 
                // Function call is required first followed by caching the result for next call with the same parameters
                printfn "%O -> Not in cache, calling function..." c
                let value = f c
                cache.Add (c, value)
                value



module ReportUtils = 

    let rec lbBVal<'T> (seri:(int*'T)[]) (target:int) (defVal:'T) =

        let rec _lbBVal (seri:(int*'T)[]) (target:int) (curDex:int) =
            if (seri.Length - 1) < curDex then  
                (target, seri.[seri.Length - 1] |> snd)
            else
                let curVal = seri.[curDex] |> fst
                if curVal > target then (target, seri.[curDex - 1] |> snd)
                else if curVal = target then (target, seri.[curDex] |> snd)
                else _lbBVal seri target (curDex + 1)
        
        if seri.Length = 0 then (target, defVal)
        else if (seri.[0] |> fst) > target then (target, defVal)
        else   _lbBVal seri target 0

    // given a set of labeled time series: seq { startingA; startingB; startingC; startingD; }
    // with missing time markers:
    // let startingA = ("a", [|(1, "a_1"); (8, "a_8");|])
    // let startingB = ("b", [|(0, "b_0"); (7, "b_7");|])
    // let startingC = ("c", [|(0, "c_0"); (3, "c_3"); (5, "c_5");|])
    // let startingD = ("d", [|(0, "d_0");|])
    //
    // .. this function will fill in the gaps, to produce a dictionary of padded series:
    //let expectedA = ("a", [|(0, ""); (1, "a_1"); (2, "a_1"); (3, "a_1"); (4, "a_1"); (5, "a_1"); (6, "a_1"); (7, "a_1"); (8, "a_8");|])
    //let expectedB = ("b", [|(0, "b_0"); (1, "b_0"); (2, "b_0"); (3, "b_0"); (4, "b_0"); (5, "b_0"); (6, "b_0"); (7, "b_7"); (8, "b_7");|])
    //let expectedC = ("c", [|(0, "c_0"); (1, "c_0"); (2, "c_0"); (3, "c_3"); (4, "c_3"); (5, "c_5"); (6, "c_5"); (7, "c_5"); (8, "c_5");|])
    //let expectedD = ("d", [|(0, "d_0"); (1, "d_0"); (2, "d_0"); (3, "d_0"); (4, "d_0"); (5, "d_0"); (6, "d_0"); (7, "d_0"); (8, "d_0");|])

    // The output series data will just be a tuple containing the outputs of dDex and dData, the starting data can be different.

    let padSeries<'H,'D> (hdTups: seq<'H*'D[]>) 
                         (dDex:'D->int)
                         (dData:'D->string)
                         (hRep:'H->string) =

        let dRep (aa:'D[]) = 
            aa |> Array.map(fun d -> (dDex d, dData d))

        let ldaMap = hdTups |> Seq.map(fun tup -> (tup |> fst |> hRep, tup |> snd |> dRep))
                            |> Map.ofSeq

        let allDexes = ldaMap.Values 
                        |> Seq.map(Array.map(fst))
                        |> Seq.concat
                        |> CollectionUtils.sortedUnique
                        |> Seq.toArray

        let maxDex = allDexes |> Array.max

        let repD = ldaMap.Keys 
                   |> Seq.map(fun k -> (k, Array.create (maxDex + 1) (-1, "")))
                   |> Map.ofSeq

        let procDex dex =
            ldaMap.Keys |> Seq.iter(fun k -> 
                            let rptV = lbBVal ldaMap.[k] dex ""
                            repD.[k].[dex] <- rptV)
            
        [|0 .. maxDex|] |> Array.iter(procDex)
        repD




module ReportUtils2 = 

    // given a set of labeled time series: seq { startingA; startingB; startingC; startingD; }
    // with missing time markers:
    // let startingA = ("a", [|(1, "a_1"); (8, "a_8");|])
    // let startingB = ("b", [|(0, "b_0"); (7, "b_7");|])
    // let startingC = ("c", [|(0, "c_0"); (3, "c_3"); (5, "c_5");|])
    // let startingD = ("d", [|(0, "d_0");|])
    //
    // .. this function will fill in the gaps, to produce a dictionary of padded series:
    //let expectedA = ("a", [|(0, ""); (1, "a_1"); (2, "a_1"); (3, "a_1"); (4, "a_1"); (5, "a_1"); (6, "a_1"); (7, "a_1"); (8, "a_8");|])
    //let expectedB = ("b", [|(0, "b_0"); (1, "b_0"); (2, "b_0"); (3, "b_0"); (4, "b_0"); (5, "b_0"); (6, "b_0"); (7, "b_7"); (8, "b_7");|])
    //let expectedC = ("c", [|(0, "c_0"); (1, "c_0"); (2, "c_0"); (3, "c_3"); (4, "c_3"); (5, "c_5"); (6, "c_5"); (7, "c_5"); (8, "c_5");|])
    //let expectedD = ("d", [|(0, "d_0"); (1, "d_0"); (2, "d_0"); (3, "d_0"); (4, "d_0"); (5, "d_0"); (6, "d_0"); (7, "d_0"); (8, "d_0");|])

    // The output series data will just be a tuple containing the outputs of dDex and dData, the starting data can be different.

    let padSeries<'H,'D> (hdTups: seq<'H*'D[]>) 
                         (dDex:'D->int)
                         (dData:'D->string)
                         (hRep:'H->string) =
        
        let rec _interpoVal (seri:(int*'T)[]) (target:int) (curDex:int) =
            if (seri.Length - 1) < curDex then  
                (target, seri.[seri.Length - 1] |> snd)
            else
                let curVal = seri.[curDex] |> fst
                if curVal > target then (target, seri.[curDex - 1] |> snd)
                else if curVal = target then (target, seri.[curDex] |> snd)
                else _interpoVal seri target (curDex + 1)


        let ldaMap = hdTups 
                     |> Seq.map(fun tup -> 
                        ( tup |> fst |> hRep, 
                          tup |> snd |> Array.map(fun d -> (dDex d, dData d)))
                        )
                     |> Map.ofSeq

        let maxDex = ldaMap.Values 
                        |> Seq.map(Array.map(fst))
                        |> Seq.concat
                        |> Seq.max

        let resMap = ldaMap.Keys 
                   |> Seq.map(fun k -> (k, Array.create (maxDex + 1) (-1, "")))
                   |> Map.ofSeq

        let procDex dex =
            ldaMap.Keys |> Seq.iter(fun k -> 
                            let rptV = _interpoVal ldaMap.[k] dex 0
                            resMap.[k].[dex] <- rptV)
            
        [|0 .. maxDex|] |> Array.iter(procDex)
        resMap
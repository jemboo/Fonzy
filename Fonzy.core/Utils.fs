namespace global
open System.Collections.Generic
open Microsoft.FSharp.Core
open System
open System.Security.Cryptography
open System.Runtime.Serialization.Formatters.Binary
open System.IO


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
        let bs = bytesForObj o
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

    let guidFromObjList (objs:obj list) =
        let acc = Array.create 16 (byte 0)
        let folder (a:byte[]) (b:byte[]) =
            a |> Array.map2(fun a b -> a+b) b

        let md5 = MD5.Create();
        let laz = objs |> List.map(fun o -> md5.ComputeHash(ByteUtils.structHash o))
                       |> List.fold(fun a b -> folder a b) acc
        System.Guid(laz)

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


module CollectionUtils =
    // Generates an n-dimenstional cartesian product, with n = LL.Length
    //let rec cart1 LL = 
    //    match LL with
    //    | [] -> Seq.singleton []
    //    | L::Ls -> seq {for x in L do for xs in cart1 Ls -> x::xs}

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

    let iterateCircular (count:int) (ofWhat:'a[]) =
        seq { for i in 0..(count-1) 
                    do yield ofWhat.[i%ofWhat.Length] }

    // Converts seq of key - value pairs to mutable Dictionary
    let dictFromSeqOfTuples(src:seq<'a * 'b>) = 
       let dictionary = new Dictionary<'a, 'b>()
       for (k,v) in src do
           dictionary.Add(k,v)
       dictionary

    // returns a list of the items that were added
    let addDictionary (dBase:Dictionary<'a, 'b>) (dAdd:Dictionary<'a, 'b>) =
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

    // returns a list of the new items added
    let cumulate (cumer:Dictionary<int, Dictionary<'a,'b>>) 
                 (key:int) (group:'a) (item:'b) =
        if cumer.ContainsKey(key) then
            cumer.[key].Add(group, item)
            [item]
        else
            let newDict = new Dictionary<'a,'b>()
            newDict.Add(group, item)
            cumer.Add(key, newDict)
            [item]

    let cumerBackFill (cumer:Dictionary<int, Dictionary<'a,'b>>) =
        let backFill (dPrev:Dictionary<'a,'b>) (dNext:Dictionary<'a,'b>)
                     (nextKey:int) =
            addDictionary dNext dPrev  |> List.map(fun a->(nextKey, a))
        let hops = cumer.Keys |> Seq.sort |> Seq.toList |> listToTransitionTuples
        let ssts = hops |> List.map(fun (p,s) -> backFill cumer.[p] cumer.[s] s)
        ssts |> List.concat
         
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


    let sortedUnique (items:int seq) =
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

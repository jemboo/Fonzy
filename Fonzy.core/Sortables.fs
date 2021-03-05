﻿namespace global
open System



type SortableIntArray = private SortableIntArray of int[]
module SortableIntArray =
    let create (intArray:int[]) = SortableIntArray intArray
    let Identity (order: int) = create [|0 .. order-1|]
    let value (SortableIntArray p) = p
    let apply f (p:SortableIntArray) = f (value p)

    let copy (sortableIntArray:SortableIntArray) = 
        create (Array.copy (value sortableIntArray))


type SortableSetRollup = {degree:Degree; baseArray:int[]; count:int}
module SortableSetRollup =
    let create (degree:Degree) (baseArray:int[] ) =
        if baseArray.Length < 0 + (Degree.value degree) then
            Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                    baseArray.Length (Degree.value degree))
        else
            let baseCopy = Array.zeroCreate baseArray.Length
            Array.Copy(baseArray, baseCopy, baseArray.Length)
            {degree=degree; 
             baseArray=baseCopy; 
             count=baseCopy.Length / (Degree.value degree) } |> Ok

    let copy (sortableSet:SortableSetRollup) =
        let baseCopy = Array.zeroCreate sortableSet.baseArray.Length
        Array.Copy(sortableSet.baseArray, baseCopy, baseCopy.Length)
        {degree=sortableSet.degree; 
         baseArray=baseCopy;
         count=baseCopy.Length / (Degree.value sortableSet.degree) } |> Ok

    let copy2 (sortableSet:SortableSetRollup) =
        let baseCopy = Array.create sortableSet.baseArray.Length 0
        Array.Copy(sortableSet.baseArray, baseCopy, baseCopy.Length)
        {degree=sortableSet.degree; 
         baseArray=baseCopy;
         count=baseCopy.Length / (Degree.value sortableSet.degree) } |> Ok

    let allBinary (degree:Degree) =
        let baseArray = IntBits.AllBinaryTestCasesArray (Degree.value degree)
                        |> Array.collect(id)
        create degree baseArray


type SortableSetExplicit = {id:Guid; degree:Degree; sortableIntArrays:SortableIntArray[]}
module SortableSetExplicit = 
    let toRollup (sse:SortableSetExplicit) = 
        let siaCount = sse.sortableIntArrays.Length
        let sroll = sse.sortableIntArrays |> Array.map(SortableIntArray.value)
                                          |> Array.collect(id)
        {
            SortableSetRollup.degree = sse.degree;
            SortableSetRollup.count = siaCount;
            SortableSetRollup.baseArray = sroll
        }


type SortableSetGenerated = {id:Guid; cat:string; prams:Map<string, string>;}



type SortableSet =
     | Explicit of SortableSetExplicit
     | Generated of SortableSetGenerated

module SortableSet = 
    let getId (ss:SortableSet) =
        match ss with
        | Explicit ess -> ess.id
        | Generated gss -> gss.id
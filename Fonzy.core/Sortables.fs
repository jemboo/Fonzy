namespace global
open System

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
                        |> Array.collect(fun a -> a)
        create degree baseArray

type IntBitsSet = {degree:Degree; intBitsA:IntBits[]}
type PermutationSet = {degree:Degree; intBitsA:IntBits[]}

type SortableSet =
     | IntBitsSet of IntBitsSet
     | PermutationSet of PermutationSet

module SortableSet = 
    let yab = None
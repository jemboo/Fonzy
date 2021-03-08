namespace global
open System


type SortableIntArray = private SortableIntArray of int[]
module SortableIntArray =
    let create (intArray:int[]) = SortableIntArray intArray
    let Identity (order: int) = create [|0 .. order-1|]
    let value (SortableIntArray p) = p
    let apply f (p:SortableIntArray) = f (value p)

    let copy (sortableIntArray:SortableIntArray) = 
        create (Array.copy (value sortableIntArray))

    //Returns a degree + 1 length array of all 
    // possible sorted 0-1 sequences of length degree
    let allSorted_0_1 (degree:Degree) =
        IntBits.Sorted_0_1_Sequences (Degree.value degree)
            |> Seq.map(create) |> Seq.toArray


type SortableSetRollup = {degree:Degree; baseArray:int[]; count:int}
module SortableSetRollup =
    let create (degree:Degree) (baseArray:int[] ) =
        if baseArray.Length < 0 + (Degree.value degree) then
            Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                    baseArray.Length (Degree.value degree))
        else
            let baseCopy = Array.zeroCreate baseArray.Length
            Array.Copy(baseArray, baseCopy, baseArray.Length)
            {
                SortableSetRollup.degree=degree; 
                baseArray=baseCopy; 
                count=baseCopy.Length / (Degree.value degree) 
            } |> Ok

    let fromSortableIntArrays (degree:Degree) 
                              (baseArrays:SortableIntArray[]) =
        result {
            let a = baseArrays |> Array.map(SortableIntArray.value)
                               |> Array.collect(id)
            return! create degree a 
        }

    let toSortableIntArrays (ssRollup:SortableSetRollup) =
        let d = (Degree.value ssRollup.degree)
        ssRollup.baseArray |> Array.chunkBySize d
                           |> Array.map(SortableIntArray.create)

    let copy (sortableSet:SortableSetRollup) =
        let baseCopy = Array.zeroCreate sortableSet.baseArray.Length
        Array.Copy(sortableSet.baseArray, baseCopy, baseCopy.Length)
        {
            SortableSetRollup.degree=sortableSet.degree; 
            baseArray=baseCopy;
            count=baseCopy.Length / (Degree.value sortableSet.degree) 
        } |> Ok

    let allBinary (degree:Degree) =
        let baseArray = IntBits.AllBinaryTestCasesArray (Degree.value degree)
                        |> Array.collect(id)
        create degree baseArray

    let isSorted (ssRollup:SortableSetRollup) =
        let d = (Degree.value ssRollup.degree)
        seq {0 .. d .. ssRollup.baseArray.Length}
            |> Seq.forall(fun dex -> Combinatorics.isSortedOffset ssRollup.baseArray dex d)
        
    let sortedCount (ssRollup:SortableSetRollup) =
        let d = (Degree.value ssRollup.degree)
        seq {0 .. d .. (ssRollup.baseArray.Length - 1)}
            |> Seq.filter (fun dex -> Combinatorics.isSortedOffset ssRollup.baseArray dex d)
            |> Seq.length

    let distinctResults (ssRollup:SortableSetRollup) =
        let d = (Degree.value ssRollup.degree)
        let chunks = seq {0 .. d .. (ssRollup.baseArray.Length - 1)}
                        |> Seq.chunkBySize d
                        |> Seq.map(Array.toList)
                        |> Seq.toArray
        chunks
        //seq {0 .. d .. (ssRollup.baseArray.Length - 1)}
        //    |> Seq.chunkBySize d
        //    |> Seq.map(Array.toList)
        //    |> Seq.distinct

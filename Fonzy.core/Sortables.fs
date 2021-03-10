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

type SortableSetRollout = {
                            degree:Degree; 
                            baseArray:int[]; 
                            sortableCount:SortableCount
                         }
module SortableSetRollout =
    let create (degree:Degree) (baseArray:int[] ) =
        if baseArray.Length < 0 + (Degree.value degree) then
            Error (sprintf "baseArray length %d is not a multiple of degree: %d:" 
                    baseArray.Length (Degree.value degree))
        else
            let baseCopy = Array.zeroCreate baseArray.Length
            Array.Copy(baseArray, baseCopy, baseArray.Length)
            {
                SortableSetRollout.degree=degree; 
                baseArray=baseCopy; 
                sortableCount= SortableCount.fromInt (baseCopy.Length / (Degree.value degree))
            } |> Ok

    let fromSortableIntArrays (degree:Degree) 
                              (baseArrays:SortableIntArray[]) =
        result {
            let a = baseArrays |> Array.map(SortableIntArray.value)
                               |> Array.collect(id)
            return! create degree a 
        }

    let toSortableIntArrays (ssRollout:SortableSetRollout) =
        let d = (Degree.value ssRollout.degree)
        ssRollout.baseArray |> Array.chunkBySize d
                           |> Array.map(SortableIntArray.create)

    let copy (sortableSetRollout:SortableSetRollout) =
        let baseCopy = Array.zeroCreate sortableSetRollout.baseArray.Length
        Array.Copy(sortableSetRollout.baseArray, baseCopy, baseCopy.Length)
        {
            SortableSetRollout.degree=sortableSetRollout.degree; 
            baseArray=baseCopy;
            sortableCount=sortableSetRollout.sortableCount
        } |> Ok

    let allBinary (degree:Degree) =
        let baseArray = IntBits.AllBinaryTestCasesArray (Degree.value degree)
                        |> Array.collect(id)
        create degree baseArray

    let isSorted (ssRollout:SortableSetRollout) =
        let d = (Degree.value ssRollout.degree)
        seq {0 .. d .. ssRollout.baseArray.Length}
            |> Seq.forall(fun dex -> Combinatorics.isSortedOffset ssRollout.baseArray dex d)
        
    let sortedCount (ssRollout:SortableSetRollout) =
        let d = (Degree.value ssRollout.degree)
        seq {0 .. d .. (ssRollout.baseArray.Length - 1)}
            |> Seq.filter (fun dex -> Combinatorics.isSortedOffset ssRollout.baseArray dex d)
            |> Seq.length

    let distinctSortableSets (ssRollout:SortableSetRollout) =
        ssRollout |> toSortableIntArrays
                 |> Seq.distinct
                 |> Seq.toArray

    let histogramOfSortableSets (ssRollout:SortableSetRollout) =
        ssRollout |> toSortableIntArrays
                 |> Seq.countBy id
                 |> Seq.toArray
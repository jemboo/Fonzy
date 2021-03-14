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

//Returns all 0-1 sequences of length degree
    let all_0_1 (degree:Degree) =
        IntBits.AllBinaryTestCasesSeq (Degree.value degree)
            |> Seq.map(create) |> Seq.toArray


type SortableSetExplicit = {id:SortableSetId; degree:Degree; 
                            sortableIntArrays:SortableIntArray[]}

type SortableSetGenerated = {id:SortableSetId; cat:string; prams:Map<string, string>;}


type SortableSet =
        | Explicit of SortableSetExplicit
        | Generated of SortableSetGenerated

module SortableSet = 
    let getId (ss:SortableSet) =
        match ss with
        | Explicit ess -> ess.id
        | Generated gss -> gss.id


module SortableSetExplicit = 

    let allIntBits (degree:Degree) (id:SortableSetId) = 
        {
              SortableSetExplicit.id = id;
              SortableSetExplicit.degree = degree;
              SortableSetExplicit.sortableIntArrays = 
                    SortableIntArray.all_0_1 degree
         }

    let rndBits (degree:Degree) 
                (rngGen:RngGen) 
                (count:int)
                (id:SortableSetId) = 
        let rando = rngGen |> Rando.fromRngGen
        let sias = Permutation.createRandoms degree rando
                    |> Seq.take count
                    |> Seq.map(fun p -> SortableIntArray.create p.values)
                    |> Seq.toArray
        {
              SortableSetExplicit.id = id;
              SortableSetExplicit.degree = degree;
              SortableSetExplicit.sortableIntArrays = sias
         }

    let rndPerms (degree:Degree) 
                 (rngGen:RngGen) 
                 (sortableCount:SortableCount)
                 (id:SortableSetId) = 
        let rando = rngGen |> Rando.fromRngGen
        let sia = Permutation.createRandoms degree rando
                    |> Seq.map(fun p -> SortableIntArray.create 
                                            (Permutation.arrayValues p))
                    |> Seq.take (SortableCount.value sortableCount)
                    |> Seq.toArray
        {
              SortableSetExplicit.id = id;
              SortableSetExplicit.degree = degree;
              SortableSetExplicit.sortableIntArrays = sia
         }



    //let toRollout (sse:SortableSetExplicit) =
    //    let sroll = sse.sortableIntArrays |> Array.map(SortableIntArray.value)
    //                                      |> Array.collect(id)
    //    {
    //        SortableSetRollout.degree = sse.degree;
    //        SortableSetRollout.sortableCount = SortableCount.fromInt 
    //                                                sse.sortableIntArrays.Length;
    //        SortableSetRollout.baseArray = sroll
    //    }

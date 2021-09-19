namespace global
open Microsoft.FSharp.Core
open System.Numerics


type sortableVec = { degree:Degree; vecLines:vecP64[] }

module SortableVec =

    let fromBitsP64 (degree:Degree)
                    (bitsP64:seq<bitsP64>) =
        let fromRz (rz:ResizeArray<uint64>) = 
            {vecP64.values = rz |> Seq.toArray}
            
        let rszFv = Array.init (Degree.value degree)
                               (fun _ -> new ResizeArray<uint64>())
        let rszAppend (bp:bitsP64) = 
            bp.values |> Array.iteri(fun i ui64 -> rszFv.[i].Add ui64)

        bitsP64 |> Seq.iter(rszAppend)
        {
            sortableVec.degree = degree;
            vecLines = Array.init
                            (Degree.value degree)
                            (fun i -> fromRz rszFv.[i] )
        }


    let fromBitSet (degree:Degree)
                   (intBits:seq<bitSet>) =
        intBits |> BitsP64.fromBitSet
                |> fromBitsP64 degree


    let toBitsP64 (svec:sortableVec) =
        let bp64ct = svec.vecLines.[0].values.Length
        let bp64Of (dex:int) 
                   (svec:sortableVec) = 
            { 
                bitsP64.values =
                  Array.init (Degree.value svec.degree)
                       (fun i -> svec.vecLines.[i].values.[dex])
            }

        seq { for i in 0 .. (bp64ct - 1) do
                yield bp64Of i svec}


    let toBitSet (svec:sortableVec) =
        svec |> toBitsP64 |> BitsP64.toBitSets


    let compEx  (svec:sortableVec) (switch:Switch) =
        let lowVec = Vector(svec.vecLines.[switch.low].values)
        let hiVec = Vector(svec.vecLines.[switch.hi].values)
        let newLowVec = Vector.BitwiseAnd(lowVec, hiVec)
        let newHiVec = Vector.BitwiseOr(lowVec, hiVec)
        let useVec = Vector.AndNot(lowVec, hiVec)
        //let useCount = Vector  //  Vector.Count useVec
        None

module FastUtils =

    let contains (value: uint64) (array: uint64[]) =
      let chunkSize = Vector<uint64>.Count
      /// ' Use SIMD registers to compare chunks of array at once
      let rec fast (cmp: Vector<uint64>) (array: uint64[]) (i: int) =
        if i > array.Length - chunkSize then slow value array i
        elif Vector.EqualsAny(cmp, Vector(array, i)) then true // compare entire chunk at once
        else fast cmp array (i+chunkSize)
        
      /// if array remainer size doesn't fit into SIMD register
      /// fallback to check array items one by one
      and slow (value: uint64) (array: uint64[]) (i: int) =
        if i >= array.Length then false
        elif array.[i] = value then true
        else slow value array (i+1)
        
      // create vector of 't filled with value copies on all positions
      let cmp = Vector(value)
      fast cmp array 0



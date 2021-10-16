namespace global
open System.Numerics

type sparseArray<'T when 'T:equality> = 
        {defaultVal:'T; len:ArrayLength; diffs:(int*'T)[]}

module SparseArray =

    let toSparse 
            (def:'T when 'T:equality)
            (baseA:'T[] when 'T:equality) =
        {
            sparseArray.defaultVal = def;
            len = (ArrayLength.fromInt baseA.Length);
            diffs = baseA |> SizeOpt.toSparseFormat def
                          |> Seq.toArray
        }

    let fromSparse<'T when 'T:equality>
                   (sa:sparseArray<'T>) =
        SizeOpt.fromSparseFormat 
            sa.defaultVal
            (ArrayLength.value sa.len) 
            sa.diffs



type sparseArraySet<'T when 'T:equality> =
    { defaultVal: 'T;
      arrayLen:ArrayLength;
      aas: sparseArray<'T>[] }

module SparseArraySet = 

    let fromStandardArraySet<'T when 'T:equality>
                   (def:'T when 'T:equality)
                   (arrayLen:ArrayLength)
                   (arrays:'T[][] when 'T:equality) =
        {
            sparseArraySet.defaultVal = def;
            sparseArraySet.arrayLen = arrayLen;
            sparseArraySet.aas = arrays |> Array.map(SparseArray.toSparse def)
        }


    let toStandardArraySet<'T when 'T:equality>
                    (sas:sparseArraySet<'T>) = 
        sas.aas |> Array.map(SparseArray.fromSparse)



type diffArray<'T when 'T:equality> = { diffs:(int*'T)[] }

module DiffArray =

    let toDiff (baseA:'T[] when 'T:equality) 
               (newA:'T[] when 'T:equality) =
        let diffs = newA |> SizeOpt.toDiffFormat baseA |> Seq.toArray
        { diffArray.diffs = diffs}


    let fromDiff<'T when 'T:equality>
                   (baseA:'T[] when 'T:equality) 
                   (dfa:diffArray<'T>) =
        SizeOpt.fromDiffFormat 
                baseA
                dfa.diffs


type diffArraySet<'T when 'T:equality> =
    { baseArray: 'T[];
      dfAs: diffArray<'T>[] }

module DiffArraySet = 

    let fromStandardArraySet<'T when 'T:equality>
                   (baseA:'T[])
                   (arrays:'T[][] when 'T:equality) =

        let yab = 
            seq {
                let mutable compy = baseA
                for dex = 0 to (arrays.Length - 1) do
                    yield arrays.[dex] |> DiffArray.toDiff compy
                    compy <- arrays.[dex]
            } 
            |> Seq.toArray

        {
            diffArraySet.baseArray = baseA;
            diffArraySet.dfAs = yab
        }


    let toStandardArraySet<'T when 'T:equality>
                    (das:diffArraySet<'T>) = 
            seq {
                let mutable compy = das.baseArray
                for dex = 0 to (das.dfAs.Length - 1) do
                    let restored = das.dfAs.[dex] |> DiffArray.fromDiff compy
                    yield restored
                    compy <- restored
            } 
            |> Seq.toArray
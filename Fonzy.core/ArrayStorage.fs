namespace global
open System.Numerics

// a permutation of the set {0, 1,.. (degree-1)}
type arrayStorage<'T when 'T:equality> =
     | Standard of 'T[]
     | Sparse of 'T*(int*'T)[]
     | Diff of (int*'T)[]


module ArrayStorage =

    let toSparse (baseA:'T[] when 'T:equality) 
                 (def:'T when 'T:equality) =
        let diffs = baseA |> SizeOpt.toSparseFormat def
                          |> Seq.toArray
        arrayStorage.Sparse (def, diffs)


    let toDiff (baseA:'T[] when 'T:equality) 
                 (newA:'T[] when 'T:equality) =
        newA |> SizeOpt.getDiffs baseA
        |> Seq.toArray |> arrayStorage.Diff

    
    let fromSparse (dnSz: ('T*int) option when 'T:equality)
                   (diffs:(int*'T)[] when 'T:equality) = 
        match dnSz with
        | Some (dfVal, arraySz) -> SizeOpt.fromSparseFormat diffs dfVal arraySz |> Ok
        | None -> "default and max" |> Error


    let fromDiff (baseA:'T[] option when 'T:equality)
                 (diffs:(int*'T)[] when 'T:equality) = 
        match baseA with
        | Some aot -> SizeOpt.restoreFromDiffs aot diffs |> Ok
        | None -> "base array missing" |> Error



type arrayOfStorageArray<'T when 'T:equality> =
    { baseArray: 'T[] option;
      dnSz:('T*int) option;
      aas: arrayStorage<'T>[] }


module ArrayOfStorageArray =


    let fromSparse (def:'T when 'T:equality)
                   (diffs:(int*'T)[] when 'T:equality) = 
        None


    let fromDiff (baseA:'T[] when 'T:equality)
                 (diffs:(int*'T)[] when 'T:equality) = 
        SizeOpt.restoreFromDiffs baseA diffs


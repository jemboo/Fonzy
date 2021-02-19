
namespace global
open System

// Ga.Sorter
type SorterFitness = private SorterFitness of float
type SorterMutationType = | Switch of MutationRate 
                          | Stage of MutationRate


module SorterFitness =
    let value (SorterFitness v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName SorterFitness -100000.0 10000.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                          |Some r -> sprintf "%.4f" (value r)
                          |None -> ""
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>float)
        }


module SorterMutationType =
    let StrF (mt:SorterMutationType) =
        match mt with
        | SorterMutationType.Switch mr -> sprintf "w%.3f" (MutationRate.value mr)
        | SorterMutationType.Stage mr -> sprintf "t%.3f" (MutationRate.value mr)
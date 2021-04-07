
namespace global
open System

type SorterFitness = private SorterFitness of float

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

    let reportEvals stats gen =
        StringUtils.printArrayf 
            (fun ((s,w,t),f) -> sprintf "%d %d %d %f" 
                                    gen (SwitchCount.value w) 
                                    (StageCount.value t) 
                                    (value f)) 
            stats



//type SorterMutationType = 
//    | Switch of MutationRate 
//    | Stage of MutationRate


//module SorterMutationType =
//    let StrF (mt:SorterMutationType) =
//        match mt with
//        | SorterMutationType.Switch mr -> sprintf "w%.3f" (MutationRate.value mr)
//        | SorterMutationType.Stage mr -> sprintf "t%.3f" (MutationRate.value mr)


//    let mutate (mutationType:SorterMutationType) (rnd:IRando) (sorter:Sorter) =
//        match mutationType with
//        | SorterMutationType.Switch mr -> SorterGen.mutateBySwitch mr rnd sorter
//        | SorterMutationType.Stage mr -> SorterGen.mutateByStage mr rnd sorter
         
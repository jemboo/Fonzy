namespace global  // note use of GLOBAL namespace

open System

type Energy = private Energy of float
module Energy =
    let value (Energy v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName Energy 0.0 10.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                          |Some r -> sprintf "%.4f" (value r)
                          |None -> ""
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>float)
        }

    let failure = 
        Double.MaxValue |> fromFloat

type Temp = private Temp of float
module Temp =
    let value (Temp v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName Temp 0.0 1.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>float)
        }

type StepNumber = private StepNumber of int
module StepNumber =
    let value (StepNumber v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName StepNumber 0 100000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let increment gen = fromInt ((value gen) + 1)
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

type annealerSpec = 
    | Constant of Temp
    | Exp of Temp * float

//type annealer = { annealerSpec:annealerSpec;
//                  chooser:Energy->Energy->(unit->float)->StepNumber->bool  }

module Annealer =

    let makeConst (temp:Temp) =
        fun curFitness newFitness (caster:unit->float) _ ->
            let ofv = curFitness |> Energy.value
            let nfv = newFitness |> Energy.value
            if (ofv >= nfv) then
                true
            else
                let tv = Temp.value temp
                let curTry = caster()
                let curThresh = Math.Exp(-(nfv - ofv) / tv)
                curTry < curThresh

        
    let makeExp (temp:Temp) (decay:float) =
        fun curFitness newFitness (caster:unit->float) step ->
            let ofv = curFitness |> Energy.value
            let nfv = newFitness |> Energy.value
            if (ofv >= nfv) then
                true
            else
                let tv = Temp.value temp
                let stepFlt = StepNumber.value step |> float
                let curtemp = tv * Math.Exp (- stepFlt / decay)
                let curTry = caster()
                let curThresh = Math.Exp(-(nfv - ofv) / curtemp)
                curTry < curThresh


    let make (annealerSpec:annealerSpec) =
        match annealerSpec with
        | Constant t -> makeConst t
        | Exp (t,hl) -> makeExp t hl


    //let make (annealerSpec:annealerSpec) =
    //    match annealerSpec with
    //    | Constant t -> { annealer.annealerSpec = annealerSpec;
    //                      chooser = (makeConst t); }
    //    | Exp (t,hl) -> { annealer.annealerSpec = annealerSpec;
    //                      chooser = (makeExp t hl) }
    

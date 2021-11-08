namespace global

open System

type Energy = private Energy of float
module Energy =
    let value (Energy v) = v
    let create fieldName v = Energy v |> Ok
       // ConstrainedType.createFloat fieldName Energy 0.0 Double. v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                          |Some r -> sprintf "%.4f" (value r)
                          |None -> ""
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>float)
        }

    let failure = Double.MaxValue |> fromFloat

    let betterEnergy (a:Energy option) 
                     (b:Energy option) = 
        match a, b  with
        | Some av, Some bv -> if (value av) < (value bv) 
                                then a else b
        | None, Some e -> Some e
        | Some e, None -> Some e
        | _, None -> failwith "energy missing"

    let isBetterThan (lhs:Energy) 
                     (rhs:Energy) = 
        (value lhs) < (value rhs)


type Temp = private Temp of float
module Temp =
    let value (Temp v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName Temp 0.0 10.0 v
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
    let ticsPerLog = 20.0
    let logReporting (StepNumber totSteps) =
        let padding = 0.5
        let logSteps = (totSteps |> float |> Math.Log2) + padding
        let ticCt =  ticsPerLog * logSteps
        Array.init (ticCt |> int) 
                   (fun dex -> (Math.Pow(2.0, ((dex |> float) * logSteps) / ticCt))
                                |> int |> fromInt)
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }


type RevNumber = private RevNumber of int
module RevNumber =
    let value (RevNumber v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName RevNumber 0 100000000 v
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

module AnnealerSpec =
    let report (a:annealerSpec) =
        match a with
        | Constant t -> sprintf "c_%0.5f" (t |> Temp.value)
        | Exp (t,d) -> sprintf "e_%0.5f_%0.0f" (t |> Temp.value) d


module Annealer =

    let makeConst (temp:Temp) =
        fun curFitness newFitness (caster:unit->float) _ ->
            let cfv = curFitness |> Energy.value
            let nfv = newFitness |> Energy.value
            if (cfv >= nfv) then
                true
            else
                let tv = Temp.value temp
                let curTry = caster()
                let curThresh = Math.Exp(-(nfv - cfv) / tv) / 2.0
                curTry < curThresh

        
    let makeExp (temp:Temp) (decay:float) =
        fun curFitness newFitness (caster:unit->float) step ->
            let cfv = curFitness |> Energy.value
            let nfv = newFitness |> Energy.value
            if (cfv >= nfv) then
                true
            else
                let tv = Temp.value temp
                let stepFlt = StepNumber.value step |> float
                let curtemp = tv * Math.Exp (- stepFlt / decay)
                let curTry = caster()
                let curThresh = Math.Exp(-(nfv - cfv) / curtemp) / 2.0
                curTry < curThresh


    let make (annealerSpec:annealerSpec) =
        match annealerSpec with
        | Constant t -> makeConst t
        | Exp (t,hl) -> makeExp t hl

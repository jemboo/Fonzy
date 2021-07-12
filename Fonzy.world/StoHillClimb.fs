namespace global
open System


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


type stoHillClimb<'T> = 
    {
       trackId:Guid;
       stepNumber:StepNumber; 
       seed:int; 
       curVal:'T;
       curFitness:Fitness;
       updater: StepNumber -> int -> 'T -> 'T * int
       evaluator: StepNumber -> 'T -> Fitness
    }

module StoHillClimb =

    let update (hist:List<stoHillClimb<'T>>) = 
        let shc = hist |> List.head
        let newVal, newSeed = 
                    shc.updater 
                        shc.stepNumber 
                        shc.seed 
                        shc.curVal
        let curFitness = shc.curFitness
                         |> Fitness.value
        let newFitness = shc.evaluator shc.stepNumber newVal
                         |> Fitness.value

        if (curFitness > newFitness) then
            let newShc =
                {
                    stoHillClimb.trackId = shc.trackId;
                    stepNumber = shc.stepNumber |> StepNumber.increment;
                    curVal = shc.curVal;
                    curFitness = shc.curFitness;
                    seed = newSeed;
                    updater = shc.updater;
                    evaluator =  shc.evaluator;
                }
            hist
        else
            let newShc =
                {
                    stoHillClimb.trackId = shc.trackId;
                    stepNumber = shc.stepNumber |> StepNumber.increment;
                    curVal = newVal;
                    curFitness = Fitness.fromFloat newFitness;
                    seed = newSeed;
                    updater = shc.updater;
                    evaluator =  shc.evaluator;
                }
            newShc::hist

module SorterClimbers =
    
    let switchMutator (mutRate:MutationRate) 
                      (skipPrefix:SwitchCount) =
        fun (step:StepNumber) (seed:int) (sorter:Sorter) ->
            let randy = Rando.fromSeed RngType.Lcg seed
            let mutant = sorter |> SorterGen.mutateBySwitch mutRate randy 
            (mutant, randy.NextPositiveInt)


    let stageMutator (mutRate:MutationRate) 
                     (skipPrefix:SwitchCount) =
        fun (step:StepNumber) (seed:int) (sorter:Sorter) ->
            let randy = Rando.fromSeed RngType.Lcg seed
            let mutant = sorter |> SorterGen.mutateByStage mutRate randy 
            (mutant, randy.NextPositiveInt)



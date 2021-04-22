namespace global
open System

[<Struct>]
type Switch = {low:int; hi:int}
module Switch =
    let switchMap = 
        [for hi=0 to 64 
            do for low=0 to hi do yield {Switch.low=low; Switch.hi=hi}]

    let getIndex (switch:Switch) =
        (switch.hi * (switch.hi + 1)) / 2 + switch.low

    let zeroSwitches =
        seq { while true do yield {Switch.low=0; Switch.hi=0}}
        
    // produces switches from only the two cycle components of the 
    // permutation
    let fromIntArray (pArray:int[]) =
            seq { for i = 0 to pArray.Length - 1 do
                    let j = pArray.[i]
                    if ((j >= i ) && (i = pArray.[j]) ) then
                            yield {Switch.low=i; Switch.hi=j} }

    let fromPermutation (p:Permutation) =
        fromIntArray (Permutation.arrayValues p)
     
    let fromTwoCyclePerm (p:TwoCyclePerm) =
        fromIntArray (TwoCyclePerm.arrayValues p)
    
    let toString (sw:Switch) =
        sprintf "(%d, %d)" sw.low sw.hi

    let switchCountForDegree (order:Degree)  =
        uint32 ((Degree.value order)*(Degree.value order + 1) / 2)

    // IRando dependent
    let randomSwitchesOfDegree (degree:Degree) 
                               (rnd:IRando) =
        let maxDex = switchCountForDegree degree
        seq { while true do 
                    let p = (int (rnd.NextUInt % maxDex))
                    yield switchMap.[p] }

    let mutateSwitches (order:Degree) 
                       (mutationRate:MutationRate) 
                       (rnd:IRando) 
                       (switches:seq<Switch>) =
        let mDex = uint32 ((Degree.value order)*(Degree.value order + 1) / 2) 
        let mutateSwitch (switch:Switch) =
            match rnd.NextFloat with
            | k when k < (MutationRate.value mutationRate) -> 
                        switchMap.[(int (rnd.NextUInt % mDex))] 
            | _ -> switch
        switches |> Seq.map(fun sw-> mutateSwitch sw)

type SwitchMapTracker = {degree:Degree; tracker:int[] }

module SwitchMapTracker =
    let create (degree:Degree) = 
        let trackArraySize = int (Switch.switchCountForDegree degree)
        {SwitchMapTracker.degree = degree;
         tracker = Array.zeroCreate trackArraySize}

    let recordSwitches (recVal:int) 
                       (switchMapTracker:SwitchMapTracker) 
                       (switches:Switch seq) = 
        switches |> Seq.iter(fun w -> 
                switchMapTracker.tracker.[Switch.getIndex w] <- recVal)
        switchMapTracker

    let recordSwitch (recVal:int) 
                     (switchMapTracker:SwitchMapTracker) 
                     (switch:Switch) = 
        let dex = Switch.getIndex switch
        if switchMapTracker.tracker.[dex] = 0 then
           switchMapTracker.tracker.[dex] <- recVal
           true
        else false

    let usedIndexes (switchMapTracker:SwitchMapTracker) =
        switchMapTracker.tracker
        |> Array.mapi(fun d v -> (d,v))
        |> Array.filter(fun tup -> (snd tup) > 0)
        |> Array.map(fst)
        |> Array.toList

    let ageTracker (switchMapTracker:SwitchMapTracker) =
        {
            SwitchMapTracker.degree = switchMapTracker.degree;
            tracker = switchMapTracker.tracker
                      |> Array.map(fun v -> v - 1)
        }

    let recordAndFilter (recVal:int) 
                        (switchMapTracker:SwitchMapTracker) 
                        (switches:Switch seq) =
        seq { for w in switches do
                if recordSwitch recVal switchMapTracker w then
                    yield w }


type Stage = {switches:Switch list; degree:Degree}
module Stage =

    // returns a list of switches found in all of the stages
    let switchIntersection (stages:Stage seq) =
        stages |> Seq.map(fun st -> (Set.ofList st.switches))
               |> Set.intersectMany
               |> Set.toList

    // returns a sequence of switches found more than once
    let switchPairwiseIntersections (stages:Stage seq) =
        seq { for stage in stages do yield! stage.switches }
        |> CollectionUtils.itemsOccuringMoreThanOnce

    // returns a sequence of switches found more than once
    let windowedSwitchPairwiseIntersections 
                                (windowSize:int) 
                                (stages:Stage seq) =
        stages |> CollectionUtils.maxWindowed windowSize
               |> Seq.map(switchPairwiseIntersections >> Seq.toList)
               //|> Seq.map(fun t -> switchPairwiseIntersections t
               //                     |> Seq.toList)
              // |> switchPairwiseIntersections

    let mergeSwitchesIntoStages (degree:Degree) 
                                (switches:seq<Switch>) =
        let mutable stageTracker = Array.init (Degree.value degree) 
                                              (fun _ -> false)
        let switchesForStage = new ResizeArray<Switch>()
        seq { 
              for sw in switches do
                 if (stageTracker.[sw.hi] || stageTracker.[sw.low] ) then
                    yield { Stage.switches = switchesForStage |> Seq.toList; 
                            Stage.degree = degree}
                    stageTracker <- Array.init (Degree.value degree) 
                                                (fun _ -> false)
                    switchesForStage.Clear()
                 stageTracker.[sw.hi] <- true
                 stageTracker.[sw.low] <- true
                 switchesForStage.Add sw
              if switchesForStage.Count > 0 then
                yield { Stage.switches=switchesForStage |> Seq.toList; 
                        degree = degree}
             }

    let getStageIndexesFromSwitches (degree:Degree) 
                                    (switches:seq<Switch>) =
        let mutable stageTracker = Array.init (Degree.value degree) 
                                              (fun _ -> false)
        let mutable curDex = 0
        seq { 
             yield curDex
             for sw in switches do
                if (stageTracker.[sw.hi] || stageTracker.[sw.low] ) then
                    yield curDex
                    stageTracker <- Array.init (Degree.value degree) 
                                                (fun _ -> false)
                stageTracker.[sw.hi] <- true
                stageTracker.[sw.low] <- true
                curDex <- curDex + 1
             yield curDex
           }

    let getStageCount (degree:Degree) (switches:seq<Switch>) =
            mergeSwitchesIntoStages degree switches 
                    |> Seq.length
                    |> StageCount.create ""

    let convertToTwoCycle (stage:Stage) =
        stage.switches |> Seq.map(fun s -> (s.low, s.hi))
                        |> TwoCyclePerm.makeFromTupleSeq stage.degree


    let mutateStage (stage:Stage) (pair:int*int) =
        let tcp = stage |> convertToTwoCycle |> TwoCyclePerm.arrayValues
        let a, b = pair
        let c = tcp.[a]
        let d = tcp.[b]
        if (a=c) && (b=d) then
            tcp.[a] <- b
            tcp.[b] <- a
        elif (a=c) then
            tcp.[a] <- b
            tcp.[b] <- a
            tcp.[d] <- d
        elif (b=d) then
            tcp.[a] <- b
            tcp.[b] <- a
            tcp.[c] <- c
        else
            tcp.[a] <- b
            tcp.[c] <- d
            tcp.[b] <- a
            tcp.[d] <- c
        let sA = Switch.fromIntArray tcp |> Seq.toList
        {switches=sA; degree=stage.degree}
        
    // IRando dependent
    let createRandom (degree:Degree) (rnd:IRando) =
        let switches = (TwoCyclePerm.makeRandomFullTwoCycle degree rnd )
                        |> Switch.fromTwoCyclePerm
        {switches=switches |> Seq.toList; degree=degree}


    let makeRandomStagedSwitchSeq (degree:Degree) 
                                  (switchFreq:SwitchFrequency) 
                                  (rnd:IRando) =
        let aa (rnd:IRando)  = 
            (TwoCyclePerm.makeRandomTwoCycle degree rnd (SwitchFrequency.value switchFreq))
                    |> Switch.fromTwoCyclePerm
        seq { while true do yield! (aa rnd) }

    
    let makeRandomFullStages (degree:Degree) 
                             (rnd:IRando) =
        makeRandomStagedSwitchSeq degree SwitchFrequency.max rnd
        |> mergeSwitchesIntoStages degree


    let randomMutate (rnd:IRando) (mutationRate:MutationRate) (stage:Stage) = 
        match rnd.NextFloat with
            | k when k < (MutationRate.value mutationRate) -> 
                        let tcp = Combinatorics.drawTwoWithoutRep stage.degree rnd
                        mutateStage stage tcp
            | _ -> stage
            

    let fullStageFromSwitches (degree:Degree) (switches: Switch seq) =
        let usedFlags = Array.init (Degree.value degree) (fun _ -> false)
        None


    let gusStagesOfDegree (stagesPfx:Stage list)
                          (stageCompSpan:StageCount)
                          (degree:Degree) 
                          (rnd:IRando) =
        let stSp = (StageCount.value stageCompSpan)
        let compStages = stagesPfx |> CollectionUtils.last stSp
        (makeRandomFullStages degree rnd) 
                   |> Seq.append
                        (compStages |> List.toSeq)
            
       // compStages
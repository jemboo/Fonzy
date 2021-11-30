namespace global
open System
open System.Runtime.CompilerServices

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


    let fromSwitches (degree:Degree) 
                     (switches:seq<Switch>) =
        let mutable stageTracker = Array.init (Degree.value degree) 
                                              (fun _ -> false)
        let switchesForStage = new ResizeArray<Switch>()
        seq { 
              for sw in switches do
                 if (sw.hi <> sw.low) then
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


    //let getStageIndexesFromSwitches (degree:Degree) 
    //                                (switches:seq<Switch>) =
    //    let mutable stageTracker = Array.init (Degree.value degree) 
    //                                          (fun _ -> false)
    //    let mutable curDex = 0
    //    seq { 
    //         yield curDex
    //         for sw in switches do
    //            if (stageTracker.[sw.hi] || stageTracker.[sw.low] ) then
    //                yield curDex
    //                stageTracker <- Array.init (Degree.value degree) 
    //                                            (fun _ -> false)
    //            stageTracker.[sw.hi] <- true
    //            stageTracker.[sw.low] <- true
    //            curDex <- curDex + 1
    //         yield curDex
    //       }


    let getStageCount (degree:Degree) 
                      (switches:seq<Switch>) =
            fromSwitches degree switches 
                    |> Seq.length
                    |> StageCount.fromInt


    let convertToTwoCycle (stage:Stage) =
        stage.switches |> Seq.map(fun s -> (s.low, s.hi))
                       |> TwoCyclePerm.makeFromTupleSeq stage.degree

                       
    let mutateStageByPair (stage:Stage) 
                          (pair:int*int) =
        let tcpM = stage |> convertToTwoCycle 
                         |> TwoCyclePerm.mutateByPair pair
        let sA = Switch.fromIntArrayAsPerm tcpM.values |> Seq.toList
        {switches=sA; degree=stage.degree}


    let mutateStageReflByPair (stage:Stage) 
                              (pairs:seq<int*int>) =

        let tcpM = stage |> convertToTwoCycle 
                         |> TwoCyclePerm.mutateByReflPair pairs
        let sA = Switch.fromIntArrayAsPerm tcpM.values |> Seq.toList
        {switches=sA; degree=stage.degree}


    // IRando dependent
    let rndSeq (degree:Degree) 
               (switchFreq:SwitchFrequency) 
               (rnd:IRando) =

        let aa (rnd:IRando)  = 
            {
                switches = TwoCyclePerm.rndTwoCycle 
                                    degree
                                    (SwitchFrequency.value switchFreq)
                                    rnd
                               |> Switch.fromTwoCyclePerm
                               |> Seq.toList;
                degree=degree
            }
        seq { while true do yield (aa rnd) }


    let rndSymmetric 
                (degree:Degree) 
                (rnd:IRando) =
        let aa (rnd:IRando)  = 
            { 
                Stage.switches = 
                    TwoCyclePerm.rndSymmetric 
                            degree
                            rnd
                    |> Switch.fromTwoCyclePerm
                    |> Seq.toList
                degree = degree
            }

        seq { while true do yield (aa rnd) }


    let randomMutate (rnd:IRando) 
                     (mutationRate:MutationRate) 
                     (stage:Stage) = 
        match rnd.NextFloat with
            | k when k < (MutationRate.value mutationRate) -> 
                        let tcp = Combinatorics.drawTwoWithoutRep 
                                                    stage.degree 
                                                    rnd
                        mutateStageByPair stage tcp
            | _ -> stage


    let randomReflMutate (rnd:IRando) 
                         (mutationRate:MutationRate) 
                         (stage:Stage) = 
        match rnd.NextFloat with
            | k when k < (MutationRate.value mutationRate) -> 
                        let tcp = seq { 
                                while true do 
                                yield 
                                    Combinatorics.drawTwoWithoutRep 
                                                    stage.degree 
                                                    rnd }
                        mutateStageReflByPair stage tcp 
            | _ -> stage


    let toBuddyStages  (stagesPfx: Stage list)
                       (stageWindowSize: StageCount)
                       (stageSeq: seq<Stage>)
                       (targetStageCount: StageCount)
                       (trialStageCount: StageCount) =

        let maxWindow = (StageCount.value stageWindowSize)
        let mutable window = stagesPfx |> CollectionUtils.last maxWindow
        let trim() =
            if window.Length = maxWindow then
                window |> CollectionUtils.first (maxWindow - 1)
            else    
                window

        let buddyCount (stage:Stage) = 
            let testWin = stage::window
            switchPairwiseIntersections testWin
                            |> Seq.length
        let mutable stagesFound = 0
        let mutable stagesTested = 0
        let appendedStageCount = (StageCount.value targetStageCount) 
                                    - stagesPfx.Length

        let stager = stageSeq.GetEnumerator()
        seq { while ((stagesFound < appendedStageCount) 
                      && 
                     (stagesTested < (StageCount.value trialStageCount) )) 
               do
               window <- trim()
               stager.MoveNext() |> ignore
               stagesTested <- stagesTested + 1
               if (buddyCount stager.Current) = 0 then
                    window <- window |> List.append [stager.Current]
                    stagesFound <- stagesFound + 1
                    yield stager.Current
             }


    let rndBuddyStages (stageWindowSize:StageCount)
                        (switchFreq:SwitchFrequency) 
                        (degree:Degree) 
                        (rnd:IRando) 
                        (stagesPfx:Stage list)  =
        let stageSeq = rndSeq degree switchFreq rnd
        let maxWindow = (StageCount.value stageWindowSize)
        let mutable window = stagesPfx |> CollectionUtils.last maxWindow
        let trim() =
            if window.Length = maxWindow then
               window |> CollectionUtils.first (maxWindow - 1)
            else    
               window

        let buddyCount (stage:Stage) = 
            let testWin = stage::window
            let ahay = switchPairwiseIntersections testWin
                                    |> Seq.toArray
            let lenny = ahay |> Seq.length
            lenny

        seq { for stage in stageSeq do
                    window <- trim()
                    if (buddyCount stage) = 0 then
                        window <- window |> List.append [stage]
                        yield stage }
                    |> Seq.append
                           (stagesPfx |> List.toSeq)


    let rec rndSymmetricBuddyStages 
                (stageWindowSize:StageCount)
                (switchFreq:SwitchFrequency) 
                (degree:Degree) 
                (rnd:IRando) 
                (stagesPfx:Stage list)
                (trialStageCount:StageCount) 
                (stageCount:StageCount) =

         let trial = toBuddyStages stagesPfx
                        stageWindowSize
                        (rndSymmetric degree rnd)
                        stageCount
                        trialStageCount
                        |> Seq.toArray
         if (trial.Length >= (StageCount.value stageCount)) then
            trial |> Array.take (StageCount.value stageCount)
         else rndSymmetricBuddyStages
                    stageWindowSize
                    switchFreq
                    degree
                    rnd
                    stagesPfx
                    trialStageCount
                    stageCount



type indexedSelector<'V> = { array:('V * int)[] }

module IndexedSelector =

    let nextIndex<'V> 
            (selector:indexedSelector<'V>) 
            (qualifier:'V->bool)
            (rnd:IRando) =

        let candies = selector.array |> Array.filter(fun tup -> tup |> fst |> qualifier)
        candies |> Rando.choose rnd |> Option.map snd



type buddyTrack = { 
                    degree:Degree;
                    traces:CircularBuffer<bool*bool>[]; 
                    buffSz:StageCount; 
                  }

module BuddyTrack =

    let make (degree:Degree) 
             (buffSz:StageCount) =
        let tSide = (Degree.value degree)
        let arrayLen = (tSide) * (tSide + 1) / 2
        let cbs = Array.init 
                    arrayLen 
                    (fun _ -> CircularBuffer<bool*bool>(
                                (false,false), 
                                (StageCount.value buffSz)))
        {
            degree = degree;
            buddyTrack.traces = cbs;
            buffSz = buffSz;
        }


    let updateCb (index:int) 
                 (lowVal:bool) 
                 (hiVal:bool)
                 (bt:buddyTrack) =
        let low, hi = bt.traces.[index].Current
        bt.traces.[index].SetCurrent (lowVal || low, hiVal || hi)


    let update (bt:buddyTrack) 
               (swDex:int) =
        let switch = Switch.switchMap.[swDex]
        let lds = Switch.lowOverlapping bt.degree switch.low |> Seq.toArray
        let hds = Switch.hiOverlapping bt.degree switch.hi |> Seq.toArray
        lds |> Array.map(fun dex -> updateCb dex true false bt) |> ignore
        hds |> Array.map(fun dex -> updateCb dex false true bt) |> ignore
        

    let prepNextStage (bt:buddyTrack) =
        for dex = 0 to (bt.traces.Length - 1) do
            let sw = Switch.switchMap.[dex]
            if(sw.hi = sw.low) then
                bt.traces.[dex].Push (true, true)
            else
                bt.traces.[dex].Push (false, false)
        bt

    let toSelector (bt:buddyTrack) =
        { indexedSelector.array = bt.traces 
                                  |> Array.mapi (fun dex v -> (v, dex)) }


    let makeQualifier (depth:StageCount) = 
        let stageDepth = (StageCount.value depth)
        fun (cb:CircularBuffer<bool*bool>) ->
            let lv, hv = cb.GetTick(0)
            if (lv || hv) then false
            else 
               cb.LastNticks(stageDepth) 
               |> Array.forall(fun (lv, hv) -> not (lv && hv))


    let makeNextStage (bt:buddyTrack) 
                      (depth:StageCount) 
                      (randy:IRando) =

        let _nextW (bt:buddyTrack) =
            let selector = toSelector bt 
            let wDex = IndexedSelector.nextIndex selector (makeQualifier depth) randy
            match wDex with
            | Some d ->  update bt d
                         Some Switch.switchMap.[d]
            | None -> None

        bt |> prepNextStage |> ignore
        seq { for dex = 0 to ((bt.degree |> Degree.maxSwitchesPerStage) - 1) do
                   let wNx = _nextW bt
                   if (wNx |> Option.isSome) then
                    yield (wNx |> Option.get)  }


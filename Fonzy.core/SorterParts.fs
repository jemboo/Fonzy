namespace global
open System

[<Struct>]
type Switch = {low:int; hi:int}
module Switch =

    let toString (sw:Switch) =
        sprintf "(%d, %d)" sw.low sw.hi

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
    
    let switchCountForDegree (order:Degree)  =
        uint32 ((Degree.value order)*(Degree.value order + 1) / 2)

    // IRando dependent
    let randomSwitchesOfDegree (degree:Degree) 
                               (rnd:IRando) =
        let maxDex = switchCountForDegree degree
        seq { while true do 
                    let p = (int (rnd.NextUInt % maxDex))
                    yield switchMap.[p] }
    

    let rndSymmetric (degree:Degree)
                     (rnd:IRando) =
        let aa (rnd:IRando)  = 
            (TwoCyclePerm.rndSymmetric 
                                degree 
                                rnd )
                    |> fromTwoCyclePerm
        seq { while true do yield! (aa rnd) }


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


    let reflect (degree:Degree) (sw:Switch) =
        let deg = (Degree.value degree)
        { Switch.low = sw.hi |> Combinatorics.reflect deg;
          Switch.hi = sw.low |> Combinatorics.reflect deg; }



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

    let getStageCount (degree:Degree) 
                      (switches:seq<Switch>) =
            fromSwitches degree switches 
                    |> Seq.length
                    |> StageCount.create ""

    let convertToTwoCycle (stage:Stage) =
        stage.switches |> Seq.map(fun s -> (s.low, s.hi))
                        |> TwoCyclePerm.makeFromTupleSeq stage.degree


    let mutateStage (stage:Stage) 
                    (pair:int*int) =
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
                        mutateStage stage tcp
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

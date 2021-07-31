namespace global
open System

module SorterGen =

    let fromTwoCycleArray (tc:TwoCyclePerm[]) =
        let switches = tc |> Seq.map(fun tc-> Switch.fromTwoCyclePerm tc)
                          |> Seq.concat |> Seq.toArray
        Sorter.fromSwitches tc.[0].degree switches
            
         
    let makeAltEvenOdd (degree:Degree) 
                       (stageCount:StageCount) =
        result {
            let! twoCycles = TwoCycleGen.makeAltEvenOdd degree (Permutation.identity degree)
                                |> Seq.take (StageCount.value stageCount)
                                |> Seq.toList
                                |> Result.sequence

            return fromTwoCycleArray (twoCycles |> List.toArray)
        }


    let oddeven_merge_sort (length:int) = 
        let mutable lret = List.Empty
        let t = Math.Ceiling (Math.Log2 (length |> float))
        let cxp = Math.Pow(2.0, t - 1.0) |> int
        let mutable p = cxp
        while (p > 0) do
            let mutable q = cxp
            let mutable r = 0
            let mutable d = p
            while (d > 0) do
                seq {0 .. (length - d - 1) }
                |> Seq.filter (fun v -> (v &&& p) = r)
                |> Seq.iter (fun v -> 
                    lret <- { Switch.low = v; Switch.hi = v + d} :: lret )
                d <- q - p
                q <- q /2
                r <- p
            p <- p / 2
        lret |> List.rev


    let oddeven_merge_sort2 (length:int) = 
        let mutable lret = List.Empty
        let t = Math.Ceiling (Math.Log2 (length |> float))
        let cxp = Math.Pow(2.0, t - 1.0) |> int
        let mutable p = cxp
        while (p > 0) do
            let mutable q = cxp
            let mutable r = 0
            let mutable d = p
            while (d > 0) do
                let mutable lstage = List.Empty
                seq {0 .. (length - d - 1) }
                |> Seq.filter (fun v -> (v &&& p) = r)
                |> Seq.iter (fun v -> 
                    lstage <- { Switch.low = v; Switch.hi = v + d} :: lstage )
                d <- q - p
                q <- q /2
                r <- p
                lret <- lstage :: lret
            p <- p / 2
        lret |> List.rev



module SorterMutate =

    let mutateBySwitch
            (mutationRate:MutationRate)
            (skipPrefix:SwitchCount)
            (rnd:IRando)
            (sorter:Sorter) =
        let prefix = sorter.switches
                     |> Array.take (SwitchCount.value skipPrefix)
        let mutatedPart = sorter.switches
                           |> Array.toSeq
                           |> Seq.skip (SwitchCount.value skipPrefix)
                           |> Switch.mutateSwitches sorter.degree mutationRate rnd
                           |> Seq.toArray
        {
            Sorter.degree = sorter.degree;
            Sorter.switchCount = sorter.switchCount;
            switches = mutatedPart |> Array.append prefix
        }


    let mutateByStage (mutationRate:MutationRate)
                      (skipPrefix:SwitchCount)
                      (rnd:IRando)
                      (sorter:Sorter) =
        let prefixStages = 
                  sorter.switches
                     |> Array.take (SwitchCount.value skipPrefix)
                     |> Stage.fromSwitches sorter.degree
                     |> Seq.toArray
        let mutantStages = 
                  sorter.switches
                     |> Array.toSeq
                     |> Seq.skip (SwitchCount.value skipPrefix)
                     |> Stage.fromSwitches sorter.degree
                     |> Seq.toArray
                     |> Array.map(Stage.randomMutate rnd mutationRate)
        let newStages = mutantStages |> Array.append prefixStages
        let newSwitches = [| for stage in newStages do yield! stage.switches |]
        {
            Sorter.degree=sorter.degree;
            switchCount = (SwitchCount.fromInt newSwitches.Length);
            switches = newSwitches
        }



type sorterRndGen = 
    | RandSwitches of Switch list * SwitchCount * Degree
    | RandStages of Switch list  * StageCount * Degree
    | RandBuddies of Switch list  * StageCount * StageWindowSize * Degree 
    | RandSymmetric of Switch list  * StageCount * Degree
    | RandSymmetricBuddies of Switch list  * StageCount * StageWindowSize * Degree


module SorterRndGen =
    
    //let fromSorterGen (sorterGen:SorterGen) = 
    //    let wcC (stageCount:StageCount) =
    //        (StageCount.value stageCount) |> StageWindowSize.fromInt

    //    match sorterGen with
    //    | SorterGen.RandSwitches (wc, d) -> ([], wc, d) |> sorterRndGen.RandSwitches
    //    | SorterGen.RandStages   (ts, d) -> ([], ts, d) |> sorterRndGen.RandStages
    //    | SorterGen.RandBuddies  (tc, wc, d) -> ([], tc, (wcC wc), d) |> sorterRndGen.RandBuddies
    //    | SorterGen.RandSymmetric   (tc, d) -> ([], tc, d) |> sorterRndGen.RandSymmetric
    //    | SorterGen.RandSymmetricBuddies  (tc, wc, d) -> ([], tc, (wcC wc), d) |> sorterRndGen.RandSymmetricBuddies
    //    | SorterGen.RandCoComp (wc, d) -> ([], wc, d) |> sorterRndGen.RandSymmetric


    let getDegree (sorterGen:sorterRndGen) =
        match sorterGen with
        | RandSwitches (_, _, d) -> d
        | RandStages   (_, _, d) -> d
        | RandBuddies  (_, _, _, d) -> d
        | RandSymmetric   (_, _, d) -> d
        | RandSymmetricBuddies  (_, _, _, d) -> d


    let getSwitchPrefix (srg:sorterRndGen) =
        match srg with
        | RandSwitches (swl, _, _) -> swl
        | RandStages   (swl, _, _) -> swl
        | RandBuddies  (swl, _, _, _) -> swl
        | RandSymmetric   (swl, _, _) -> swl
        | RandSymmetricBuddies  (swl, _, _, _) -> swl


    let getSwitchCount (srg:sorterRndGen) =
        match srg with
        | RandSwitches (_, w, _) -> w
        | RandStages   (_, t, d) -> t |> StageCount.toSwitchCount d
        | RandBuddies  (_, t, _, d) -> t |> StageCount.toSwitchCount d
        | RandSymmetric   (_, t, d) -> t |> StageCount.toSwitchCount d
        | RandSymmetricBuddies  (_, t, _, d) -> t |> StageCount.toSwitchCount d


    let reportString (id:Guid)
                     (sorterRndGen:sorterRndGen) =

        match sorterRndGen with
        | RandSwitches (pfxc, wc, d) ->   
                sprintf "%s\tRandSwitches\t%s\t%d\t@\t%d"
                            (id.ToString())
                            (pfxc.Length |> string)
                            (SwitchCount.value wc) 
                            (Degree.value d)

        | RandStages (pfxc, tc, d) -> 
                sprintf "%s\tRandStages\t%s\t%d\t@\t%d"
                            (id.ToString())
                            (pfxc.Length |> string)
                            (StageCount.value tc) 
                            (Degree.value d)
  
        | RandBuddies (pfxc, tc, wc, d) ->
                sprintf "%s\tRandBuddies\t%s\t%d\t%d\t%d"
                            (id.ToString())
                            (pfxc.Length |> string)
                            (StageCount.value tc) 
                            (StageWindowSize.value wc)
                            (Degree.value d)

        | RandSymmetric (pfxc, tc, d) ->
                sprintf "%s\tRandSymmetric\t%s\t%d\t@\t%d"
                            (id.ToString())
                            (pfxc.Length |> string)
                            (StageCount.value tc) 
                            (Degree.value d)

        | RandSymmetricBuddies (pfxc, tc, wc, d) ->
                sprintf "%s\tRandSymmetricBuddies\t%s\t%d\t%d\t%d"
                            (id.ToString())
                            (pfxc.Length |> string)
                            (StageCount.value tc) 
                            (StageWindowSize.value wc)
                            (Degree.value d)


    // shorten the length of wSfx by the length of wPfx
    let fromSwitchesAndPrefix
            (degree:Degree)
            (wPfx: Switch seq)
            (wSfx: Switch seq) =
        let switches = 
            let aPfx = wPfx |> Seq.toArray
            let aSfx = wSfx |> Seq.toArray
            let aTrim = aSfx |> Array.take(aSfx.Length - aPfx.Length)
            seq { 
                    yield! aPfx
                    yield! aTrim
                }
            |> Seq.toArray
        Sorter.fromSwitches degree switches
            

    let fromTwoCyclePerms 
                (wPfx: Switch seq) 
                (tc:TwoCyclePerm[]) =
        let switches = tc |> Seq.map(fun tc-> Switch.fromTwoCyclePerm tc)
                          |> Seq.concat
        fromSwitchesAndPrefix tc.[0].degree wPfx switches
        

    let makeAltEvenOdd (degree:Degree) 
                       (stageCount:StageCount) 
                       (wPfx: Switch seq) =
        result {
            let! twoCycles = TwoCycleGen.makeAltEvenOdd 
                                    degree 
                                    (Permutation.identity degree)
                            |> Seq.take (StageCount.value stageCount)
                            |> Seq.toList
                            |> Result.sequence

            return fromTwoCyclePerms wPfx (twoCycles |> List.toArray)
        }


    // IRando dependent
    let randomStages (degree:Degree) 
                     (wPfx: Switch seq) 
                     (stageCount:StageCount)
                     (switchFreq:SwitchFrequency) 
                     (rando:IRando) =
        let switches = (Stage.rndSeq degree switchFreq rando)
                        |> Seq.take (StageCount.value stageCount)
                        |> Seq.map (fun st -> st.switches)
                        |> Seq.concat
        fromSwitchesAndPrefix degree wPfx switches


    let randomBuddies (degree:Degree) 
                      (wPfx: Switch seq) 
                      (stageCount:StageCount)
                      (stageWindowSize:StageWindowSize) 
                      (rando:IRando) =
        
        let sc = (StageCount.fromInt (StageWindowSize.value stageWindowSize))
        let switches = (Stage.rndBuddyStages 
                                sc 
                                SwitchFrequency.max  
                                degree 
                                rando
                                List.empty)
                        |> Seq.take (StageCount.value stageCount)
                        |> Seq.collect(fun st -> st.switches |> List.toSeq)
        fromSwitchesAndPrefix degree wPfx switches


    let randomReflSymmetricBuddies (degree:Degree) 
                                   (wPfx: Switch seq) 
                                   (stageCount:StageCount)
                                   (stageWindowSize:StageWindowSize) 
                                   (rando:IRando) =

        let stageTrials = ( (StageCount.value stageCount) * 100 ) |> StageCount.fromInt
        let sc = (StageCount.fromInt (StageWindowSize.value stageWindowSize))
        let switches = (Stage.rndSymmetricBuddyStages
                                sc
                                SwitchFrequency.max
                                degree 
                                rando
                                List.empty
                                stageTrials
                                stageCount)
                        |> Seq.collect(fun st -> st.switches |> List.toSeq)
                        |> Seq.toArray

        fromSwitchesAndPrefix degree wPfx switches


    let randomConjugatesOfEvenOdd (degree:Degree) 
                                  (wPfx: Switch seq) 
                                  (stageCount:StageCount) 
                                  (iRando:IRando) =
        result {
            let perms = List.init 
                            ((StageCount.value stageCount) / 2)
                            (fun _ -> TwoCyclePerm.rndFullTwoCycle degree iRando)
                        |> List.map (TwoCyclePerm.toPermutation)

            let! stp = perms |> TwoCycleGen.makeCoConjugateEvenOdd
            let atp = stp |> Seq.toArray
            return fromTwoCyclePerms wPfx atp
        }


    let randomSymmetric (degree:Degree) 
                        (wPfx: Switch seq) 
                        (stageCount:StageCount) 
                        (iRando:IRando) =
        result {
            let tcas = Array.init 
                            (StageCount.value stageCount)
                            (fun _ -> TwoCyclePerm.rndSymmetric degree iRando)

            return fromTwoCyclePerms wPfx tcas
        }

    let randomSwitches (degree:Degree) 
                       (wPfx: Switch seq) 
                       (switchCount:SwitchCount) 
                       (rnd:IRando) =
        let switches = Switch.rndNonDegenSwitchesOfDegree degree rnd
                    |> Seq.take (SwitchCount.value switchCount)
                    |> Seq.toArray
        fromSwitchesAndPrefix degree wPfx switches


    let createRandom (sorterGen:sorterRndGen) 
                     (randy:IRando) =
        match sorterGen with
        | sorterRndGen.RandSwitches  (swl, switchCount, degree) -> 
            randomSwitches 
                    degree
                    swl
                    switchCount 
                    randy

        | sorterRndGen.RandStages (swl, stageCount, degree) ->
            let sc = SwitchFrequency.fromFloat 1.0
            randomStages 
                    degree 
                    swl
                    stageCount 
                    sc
                    randy

        | sorterRndGen.RandBuddies (swl, stageCount, windowSize, degree) ->
            randomBuddies
                           degree
                           swl
                           stageCount
                           windowSize
                           randy

        | sorterRndGen.RandSymmetric (swl, stageCount, degree) ->
            randomSymmetric 
                           degree
                           swl
                           stageCount
                           randy
            |> Result.ExtractOrThrow

        | sorterRndGen.RandSymmetricBuddies (swl, stageCount, windowSize, degree) ->
            randomReflSymmetricBuddies
                           degree
                           swl
                           stageCount
                           windowSize
                           randy

    let createRandomArray (sorterRndGen:sorterRndGen)
                          (sorterCount:SorterCount)
                          (rnd:IRando) =
            (seq {1 .. (SorterCount.value sorterCount)} 
                    |> Seq.map(fun _ -> (createRandom sorterRndGen rnd))
                    |> Seq.toArray)

    let createRandomArrayP (sorterRndGen:sorterRndGen)
                           (sorterCount:SorterCount)
                           (rnd:IRando) =
        Array.init (SorterCount.value sorterCount)
                   (fun _ -> Rando.fromSeed RngType.Lcg (RandomSeed.fromInt rnd.NextPositiveInt))
                |> Array.Parallel.map
                            (fun r -> createRandom sorterRndGen r)


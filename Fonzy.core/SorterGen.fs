namespace global
open System

type SorterGen = 
    | RandSwitches of SwitchCount * Degree
    | RandStages of StageCount * Degree
    | RandCoComp of StageCount * Degree
    | RandBuddies of StageCount * StageCount * Degree 
                    //2nd StageCount is window size
    | RandSymmetric of StageCount * Degree
    | RandSymmetricBuddies of StageCount * StageCount * Degree
                    //2nd StageCount is window size


module SorterGen =

    let getDegree (sorterGen:SorterGen) =
        match sorterGen with
        | RandSwitches (_, d) -> d
        | RandStages   (_, d) -> d
        | RandCoComp   (_, d) -> d
        | RandBuddies  (_, _, d) -> d
        | RandSymmetric   (_, d) -> d
        | RandSymmetricBuddies  (_, _, d) -> d


    let reportString (sorterGen:SorterGen) =
        match sorterGen with
        | RandSwitches (wc, d) ->   
                        sprintf "RandSwitches\t%d\t@\t%d" 
                            (SwitchCount.value wc) 
                            (Degree.value d)
        | RandStages (tc, d) -> 
                        sprintf "RandStages\t%d\t@\t%d" 
                            (StageCount.value tc) 
                            (Degree.value d)
        | RandCoComp (tc, d) ->   
                        sprintf "RandCoComp\t%d\t@\t%d" 
                            (StageCount.value tc) 
                            (Degree.value d)
        | RandBuddies (tc, wc, d) -> 
                        sprintf "RandBuddies\t%d\t%d\t%d" 
                            (StageCount.value tc) 
                            (StageCount.value wc) 
                            (Degree.value d) 
        | RandSymmetric (tc, d) ->   
                        sprintf "RandSymmetric\t%d\t@\t%d"
                            (StageCount.value tc) 
                            (Degree.value d)

        | RandSymmetricBuddies (tc, wc, d) -> 
                        sprintf "RandSymmetricBuddies\t%d\t%d\t%d" 
                            (StageCount.value tc) 
                            (StageCount.value wc) 
                            (Degree.value d) 


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


    // IRando dependent

    let randomStages (degree:Degree) 
                     (stageCount:StageCount)
                     (switchFreq:SwitchFrequency) 
                     (rando:IRando) =
        let stages = Stage.rndSeq degree switchFreq rando
                        |> Seq.take (StageCount.value stageCount)
        Sorter.fromStages degree stages


    let randomBuddies (degree:Degree) 
                      (stageCount:StageCount)
                      (stageWindowSize:StageCount) 
                      (rando:IRando) =

        let switches = (Stage.rndBuddyStages 
                                stageWindowSize 
                                SwitchFrequency.max  
                                degree 
                                rando
                                List.empty)
                        |> Seq.take (StageCount.value stageCount)
                        |> Seq.collect(fun st -> st.switches |> List.toSeq)
                        |> Seq.toArray

        Sorter.fromSwitches degree switches


    let randomReflSymmetricBuddies (degree:Degree) 
                                   (stageCount:StageCount)
                                   (stageWindowSize:StageCount) 
                                   (rando:IRando) =

        let stageTrials = ( (StageCount.value stageCount) * 100 ) 
                          |> StageCount.fromInt
        let switches = (Stage.rndSymmetricBuddyStages
                                stageWindowSize
                                SwitchFrequency.max
                                degree 
                                rando
                                List.empty
                                stageTrials
                                stageCount)
                        |> Seq.collect(fun st -> st.switches |> List.toSeq)
                        |> Seq.toArray

        Sorter.fromSwitches degree switches


    let randomConjugatesOfEvenOdd (degree:Degree) 
                                  (stageCount:StageCount) 
                                  (iRando:IRando) =
        result {
            let perms = List.init 
                            ((StageCount.value stageCount) / 2)
                            (fun _ -> TwoCyclePerm.rndFullTwoCycle degree iRando)
                        |> List.map (TwoCyclePerm.toPermutation)

            let! stp = perms |> TwoCycleGen.makeCoConjugateEvenOdd
            let atp = stp |> Seq.toArray
            return fromTwoCycleArray atp
        }


    let randomSymmetric (degree:Degree) 
                        (stageCount:StageCount) 
                        (iRando:IRando) =
        result {
            let tcas = Array.init 
                            (StageCount.value stageCount)
                            (fun _ -> TwoCyclePerm.rndSymmetric degree iRando)

            return fromTwoCycleArray tcas
        }


    let randomSwitches (degree:Degree) 
                       (switchCount:SwitchCount) 
                       (rnd:IRando) =
        let switches = Switch.rndNonDegenSwitchesOfDegree degree rnd
                    |> Seq.take (SwitchCount.value switchCount)
                    |> Seq.toArray
        Sorter.fromSwitches degree switches


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


    let createRandom (sorterGen:SorterGen) 
                     (randy:IRando) =
        match sorterGen with
        | SorterGen.RandSwitches  (switchCount, degree) -> 
            randomSwitches 
                    degree 
                    switchCount 
                    randy
        | SorterGen.RandStages (stageCount, degree) ->
            let sc = SwitchFrequency.fromFloat 1.0
            randomStages 
                    degree 
                    stageCount 
                    sc
                    randy
        | SorterGen.RandCoComp (stageCount, degree) ->
            randomConjugatesOfEvenOdd 
                           degree 
                           stageCount
                           randy
            |> Result.ExtractOrThrow
        | SorterGen.RandBuddies (stageCount, windowSize, degree) ->
            randomBuddies
                           degree 
                           stageCount
                           windowSize
                           randy
        | SorterGen.RandSymmetric (stageCount, degree) ->
            randomSymmetric 
                           degree 
                           stageCount
                           randy
            |> Result.ExtractOrThrow
        | SorterGen.RandSymmetricBuddies (stageCount, windowSize, degree) ->
            randomReflSymmetricBuddies
                           degree 
                           stageCount
                           windowSize
                           randy


    let createRandomArray (sorterGen:SorterGen)
                          (sorterCount:SorterCount)
                          (rnd:IRando) =
            (seq {1 .. (SorterCount.value sorterCount)} 
                    |> Seq.map(fun _ -> (createRandom sorterGen rnd))
                    |> Seq.toArray)


    let createRandomArrayP (sorterGen:SorterGen)
                           (sorterCount:SorterCount)
                           (rnd:IRando) =
        Array.init (SorterCount.value sorterCount)
                   (fun _ -> Rando.fromSeed RngType.Lcg rnd.NextPositiveInt)
                |> Array.Parallel.map
                            (fun r -> createRandom sorterGen r)


type sorterRndGen = 
    | RandSwitches of Switch list * SwitchCount * Degree
    | RandStages of Switch list  * StageCount * Degree
    | RandBuddies of Switch list  * StageCount * StageWindowSize * Degree 
    | RandSymmetric of Switch list  * StageCount * Degree
    | RandSymmetricBuddies of Switch list  * StageCount * StageWindowSize * Degree


module SorterRndGen =

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


    let reportString (sorterGen:sorterRndGen) (pfxDescr:string) =
        match sorterGen with
        | RandSwitches (pfxc, wc, d) ->   
                sprintf "RandSwitches\t%s\t@\t%d"
                            pfxDescr
                            (Degree.value d)
        | RandStages (pfxc, tc, d) -> 
                sprintf "RandStages\t%s\t@\t%d"
                            pfxDescr
                            (Degree.value d)
  
        | RandBuddies (pfxc, tc, wc, d) ->
                sprintf "RandBuddies\t%s\t%d\t%d"
                            pfxDescr
                            (StageWindowSize.value wc)
                            (Degree.value d)

        | RandSymmetric (pfxc, tc, d) ->
                        sprintf "RandSymmetric\t%s\t@\t%d"
                                    pfxDescr
                                    (Degree.value d)

        | RandSymmetricBuddies (pfxc, tc, wc, d) ->
                sprintf "RandSymmetricBuddies\t%s\t%d\t%d"
                                        pfxDescr
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


    let mutateBySwitch (mutationRate:MutationRate) 
                       (rnd:IRando) (sorter:Sorter) =
        {
            Sorter.degree = sorter.degree;
            Sorter.switchCount = sorter.switchCount;
            switches = (Switch.mutateSwitches sorter.degree mutationRate rnd sorter.switches) 
                        |> Seq.toArray
        }

    let mutateByStage (mutationRate:MutationRate) 
                      (rnd:IRando) 
                      (sorter:Sorter) =
        let stages = Stage.fromSwitches sorter.degree sorter.switches |> Seq.toArray
        let newStages = stages |> Array.map(fun st -> st |> Stage.randomMutate rnd mutationRate)
        let newSwitches = [| for stage in newStages do yield! stage.switches |]
        {
            Sorter.degree=sorter.degree;
            switchCount = (SwitchCount.create "" newSwitches.Length) 
                            |> Result.ExtractOrThrow;
            switches = newSwitches
        }

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
                   (fun _ -> Rando.fromSeed RngType.Lcg rnd.NextPositiveInt)
                |> Array.Parallel.map
                            (fun r -> createRandom sorterRndGen r)


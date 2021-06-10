namespace global
open System

type Sorter = {degree:Degree; 
               switches:array<Switch>; 
               switchCount:SwitchCount}
module Sorter =
    let makeId (s:Sorter) = 
        let gu = [s :> obj] |> GuidUtils.guidFromObjList
        SorterId.fromGuid gu

    let create (degree:Degree) (switches:seq<Switch>) =
        let switchArray = switches |> Seq.toArray
        let switchCount = SwitchCount.fromInt switchArray.Length
        {
            Sorter.degree=degree;
            switchCount=switchCount;
            switches = switchArray
        }
   
    let appendSwitches (switches:seq<Switch>) (sorter:Sorter) =
        let newSwitches = (switches |> Seq.toArray) |> Array.append sorter.switches
        let newSwitchCount = SwitchCount.create "" newSwitches.Length |> Result.toOption
        {
            Sorter.degree = sorter.degree;
            switchCount=newSwitchCount.Value;
            switches = (switches |> Seq.toArray) |> Array.append sorter.switches
        }

    let trimLength (sorter:Sorter) (newLength:SwitchCount) =
        if (SwitchCount.value sorter.switchCount) < (SwitchCount.value newLength) then
            "New length is longer than sorter" |> Error
        else
        let newSwitches = sorter.switches |> Array.take (SwitchCount.value newLength)
        {
            Sorter.degree = sorter.degree;
            switchCount = newLength;
            switches = newSwitches
        } |> Ok


//type SorterModel = | Switches of Switch[] * Degree
//                   | TwoCyclePerms of TwoCyclePerm[]
//                  // | Set of SorterModel[] * Degree

//module SorterModel =
//    let createWithSwitchArray (degree:Degree) 
//                              (switches:Switch[]) =
//        SorterModel.Switches (switches, degree)

//    let createWithTwoCyclePermArray (twoCyclePerms:TwoCyclePerm[]) =
//        SorterModel.TwoCyclePerms (twoCyclePerms)

//    let createRandom (degree:Degree) 
//                     (switchOrStageCount:SwitchOrStageCount) 
//                     (rnd:IRando) =
//        match switchOrStageCount with
//        | SwitchOrStageCount.Switch switchCount -> 
//            let switches = Switch.randomSwitchesOfDegree degree rnd
//                        |> Seq.take (SwitchCount.value switchCount)
//                        |> Seq.toArray 
//            createWithSwitchArray degree switches

//        | SwitchOrStageCount.Stage stageCount ->
//            let perms = Array.init 
//                            (StageCount.value stageCount) 
//                            (fun _ -> TwoCyclePerm.makeRandomTwoCycle 
//                                            degree rnd 1.0)
//            createWithTwoCyclePermArray perms


    //let createWithSetOfSwitchArrays (degree:Degree) 
    //                                (switchArrays:Switch[] seq) =
    //    let a = switchArrays |> Seq.map(fun sa ->
    //               createWithSwitchArray degree sa )
    //               |> Seq.toArray
    //    SorterModel.Set (a, degree)

    //let createWithSetOfTwoCyclePermArrays 
    //                            (degree:Degree) 
    //                            (twoCyclePermArrays:TwoCyclePerm[] seq) =
    //    let a = twoCyclePermArrays |> Seq.map(fun tcpa ->
    //               createWithTwoCyclePermArray degree tcpa )
    //               |> Seq.toArray
    //    SorterModel.Set (a, degree)



type SorterGen = | RandSwitches of SwitchCount * Degree
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
                        sprintf "RandSwitches\t@\t%d" (Degree.value d)
        | RandStages (tc, d) -> 
                        sprintf "RandStages\t@\t%d" (Degree.value d)
        | RandCoComp (tc, d) ->   
                        sprintf "RandCoComp\t@\t%d" (Degree.value d)
        | RandBuddies (tc, wc, d) -> 
                        sprintf "RandBuddies\t%d\t%d" (StageCount.value wc) 
                                                      (Degree.value d) 
        | RandSymmetric (tc, d) ->   
                        sprintf "RandSymmetric\t@\t%d" (Degree.value d)

        | RandSymmetricBuddies (tc, wc, d) -> 
                        sprintf "RandSymmetricBuddies\t%d\t%d" (StageCount.value wc) 
                                                               (Degree.value d) 


    let fromTwoCycleArray (tc:TwoCyclePerm[]) =
        let switches = tc |> Seq.map(fun tc-> Switch.fromTwoCyclePerm tc)
                            |> Seq.concat |> Seq.toArray
        Sorter.create tc.[0].degree switches
            
         
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
        let switches = (Stage.makeRandomStagedSwitchSeq degree switchFreq rando)
                        |> Seq.take ((StageCount.value stageCount) * (Degree.value degree) / 2)
                        |> Seq.toArray
        Sorter.create degree switches


    let randomBuddies (degree:Degree) 
                      (stageCount:StageCount)
                      (stageWindowSize:StageCount) 
                      (rando:IRando) =

        let switches = (Stage.makeBuddyStages 
                                stageWindowSize 
                                SwitchFrequency.max  
                                degree 
                                rando
                                List.empty)
                        |> Seq.take (StageCount.value stageCount)
                        |> Seq.collect(fun st -> st.switches |> List.toSeq)
                        |> Seq.toArray

        Sorter.create degree switches


    let randomReflSymmetricBuddies (degree:Degree) 
                                   (stageCount:StageCount)
                                   (stageWindowSize:StageCount) 
                                   (rando:IRando) =

        let switches = (Stage.reflSymmetricBuddyStages
                                stageWindowSize
                                degree 
                                rando
                                List.empty)
                        |> Seq.take (StageCount.value stageCount)
                        |> Seq.collect(fun st -> st.switches |> List.toSeq)
                        |> Seq.toArray

        Sorter.create degree switches


    let randomConjugatesOfEvenOdd (degree:Degree) 
                                  (stageCount:StageCount) 
                                  (iRando:IRando) =
        result {
            let perms = List.init 
                            ((StageCount.value stageCount) / 2)
                            (fun _ -> TwoCyclePerm.makeRandomFullTwoCycle degree iRando)
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
                            (fun _ -> TwoCyclePerm.makeReflSymmetric degree iRando)

            return fromTwoCycleArray tcas
        }


    let randomSwitches (degree:Degree) 
                       (switchCount:SwitchCount) 
                       (rnd:IRando) =
        let switches = Switch.randomSwitchesOfDegree degree rnd
                    |> Seq.take (SwitchCount.value switchCount)
                    |> Seq.toArray
        Sorter.create degree switches


    let mutateBySwitch (mutationRate:MutationRate) (rnd:IRando) (sorter:Sorter) =
        {
            Sorter.degree = sorter.degree;
            Sorter.switchCount = sorter.switchCount;
            switches = (Switch.mutateSwitches sorter.degree mutationRate rnd sorter.switches) 
                        |> Seq.toArray
        }

    let mutateByStage (mutationRate:MutationRate) 
                      (rnd:IRando) 
                      (sorter:Sorter) =
        let stages = Stage.mergeSwitchesIntoStages sorter.degree sorter.switches |> Seq.toArray
        let newStages = stages |> Array.map(fun st -> st |> Stage.randomMutate rnd mutationRate)
        let newSwitches = [| for stage in newStages do yield! stage.switches |]
        {
            Sorter.degree=sorter.degree;
            switchCount = (SwitchCount.create "" newSwitches.Length) 
                            |> Result.ExtractOrThrow;
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
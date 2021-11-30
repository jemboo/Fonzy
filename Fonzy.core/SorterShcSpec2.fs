namespace global
open System



type sorterShcSpec2 = 
    {
       rngGen:RngGen; 
       sorter:sorter;
       mutatorSpec:sorterMutSpec;
       srtblSetType:sortableSetType;
       sorterStageWeightSpec:sorterStageWeightSpec;
       evalSpec: sorterEvalSpec;
       annealerSpec:annealerSpec;
       termSpec:shcTermSpec;
    }


module SorterShcSpec2 = 

    let makeId (s:sorterShcSpec2) = 
        let gu = seq {
                       s.annealerSpec :> obj;
                       s.evalSpec :> obj;
                       s.mutatorSpec :> obj; 
                       s.rngGen :> obj;
                       s.sorterStageWeightSpec :> obj; 
                       s.sorter :> obj;
                       s.srtblSetType :> obj;
                       s.termSpec :> obj; } 
                  |> GuidUtils.guidFromObjs
        ShcId.fromGuid gu

    let sorterReport (s:sorterShcSpec2) =
        s.sorter |> Sorter.makeId |> SorterId.value |> string

    let mutReport (s:sorterShcSpec2) =
        s.mutatorSpec |> SorterMutSpec.colHdr

    let seedReport (s:sorterShcSpec2) =
        s.rngGen.seed |> RandomSeed.value |> string

    let tempReport (s:sorterShcSpec2) =
        s.annealerSpec |> AnnealerSpec.report


type sorterShcSpecRndGen2 = 
    {
       baseSpec:sorterShcSpec2;
       sssrgType:sssrgType;
       rndGen:RngGen;
       count:ShcCount
    }

module SorterShcSpecRndGen2 =

    let swapAnnealers  rndG 
                       (shc:sorterShcSpec2)
                       (count: ShcCount)
                       (endPt:annealerSpec) = 
        let annAc randy =
            result {
               return! 
                   match shc.annealerSpec, endPt with
                   | annealerSpec.Constant c1, annealerSpec.Constant c2  -> 
                        Combinatorics.draw1D (Temp.value c1) (Temp.value c2) randy
                        |> Seq.map(fun t -> (Temp.fromFloat t) |> annealerSpec.Constant)
                        |> Seq.take (ShcCount.value count)
                        |> Ok
                   | annealerSpec.Exp (t1, d1), annealerSpec.Exp (t2, d2) -> 
                        Combinatorics.draw2D (Temp.value t1) d1 
                                             (Temp.value t2) d2 randy
                        |> Seq.map(fun tup -> ((Temp.fromFloat (fst tup)), (snd tup)) |> annealerSpec.Exp)
                        |> Seq.take (ShcCount.value count)
                        |> Ok
                   | _ -> "annealerSpecs must me the same type" |> Error
            }

        result {
            let randy = rndG |> Rando.fromRngGen
            let! anns = annAc randy
            return anns 
                    |> Seq.map(fun an ->  
                        { shc with 
                              rngGen = (randy |> Rando.nextRngGen);
                              annealerSpec = an })
        }


    let swapMut rndG 
                (shc:sorterShcSpec2)
                (count: ShcCount)
                (smc:sorterMutSpec) = 
        "Not impl" |> Error


    let swapRndGen rndG 
                   (shc:sorterShcSpec2)
                   (count: ShcCount) = 
        let randy = rndG |> Rando.fromRngGen
        seq {0 .. ((ShcCount.value count) - 1) }
        |> Seq.map( fun _ -> 
                { shc with 
                      rngGen = (randy |> Rando.nextRngGen)})


    let swapSorters (srSrepo: (SorterSetId->sorterSet) option) 
                    rndG 
                    (baseSpec:sorterShcSpec2) 
                    (count: ShcCount)
                    (ssg:sorterSetGen) = 
        result {
            let randy = rndG |> Rando.fromRngGen
            let! srtrSet = SorterSetGen.createSorterSet srSrepo ssg
            let srtrA = srtrSet.sorters |> Map.toArray |> Array.map(snd)
            return seq { 0 .. ((ShcCount.value count) - 1) }
            |> Seq.map( fun dex -> 
                    { baseSpec with 
                          rngGen = (randy |> Rando.nextRngGen);
                          sorter = srtrA.[dex % srtrA.Length]})
        }


    let swapStageWeight rndG 
                       (shc:sorterShcSpec2)
                       (count: ShcCount)
                       (sws:sorterStageWeightSpec) = 
        "Not impl" |> Error



    let generate (sbSrepo: (SortableSetId->sorterSet) option) 
                 (srSrepo: (SorterSetId->sorterSet) option) 
                 (sssrg:sorterShcSpecRndGen2) = 
        match sssrg.sssrgType with

        | Annealer annSpec -> swapAnnealers 
                                sssrg.rndGen 
                                sssrg.baseSpec 
                                sssrg.count 
                                annSpec

        | Mutation mutSpec -> swapMut 
                                sssrg.rndGen 
                                sssrg.baseSpec 
                                sssrg.count 
                                mutSpec

        | RndGen -> swapRndGen 
                               sssrg.rndGen
                               sssrg.baseSpec
                               sssrg.count |> Ok

        | Sorters ssG -> swapSorters srSrepo
                                     sssrg.rndGen 
                                     sssrg.baseSpec 
                                     sssrg.count 
                                     ssG

        | StageWeight stw -> swapStageWeight 
                                    sssrg.rndGen 
                                    sssrg.baseSpec 
                                    sssrg.count 
                                    stw


type sorterShcResult2 =
    {
        id:ShcId;
        msg:string;
        archive: sorterShcArch;
    }

type sorterShcResults2 =
    {
        members: array<sorterShcResult2>
    }

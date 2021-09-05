namespace global
open System

module SorterShcCauseSpecGen =

    let makeCauseSpec 
                    sorterRndGen
                    sorterCount
                    rndGen
                    switchUsePlan
                    sortableSetSpec
                    useParallel
                    resultsName =

        CauseSpecSorters.rndStoHillClimb
                ("sorterRndGen", sorterRndGen)
                ("sorterCount", sorterCount)
                ("rndGen", rndGen)
                ("switchUsePlan", switchUsePlan)
                ("sortableSetSpec", sortableSetSpec)
                ("useParallel", (UseParallel.value useParallel))
                ("resultsName", resultsName)






    let makeRunBatchSeq (seed:RandomSeed)
                        (batchSize:int)
                        (outputDir:FilePath) = 

        let degree = Degree.fromInt 12
        let maxSteps = StepNumber.fromInt 10
        let stageWght = StageWeight.fromFloat 1.0
        let temp = Temp.fromFloat 1.0
        let prefix = [||]
        let pfxLen = SwitchCount.fromInt prefix.Length
        let mutRate = MutationRate.fromFloat 0.1
        let mutType = sorterMutationType.ByStage (pfxLen, mutRate)


        let stgWghtSpec = stageWght |> shcStageWeightSpec.Constant
        let evaluatorSpec = sorterEvalSpec.PerfBin
        let annSpec = temp |> annealerSpec.Constant
        let updaterSpec = shcSaveDetails.Always
        let termSpec = maxSteps |> shcTermSpec.FixedLength
        let mutSpec = mutType |> sorterMutSpec.Constant


        let allSorterGens = 
                //(makeRandSwitches degreesToTest) |> List.append
                // (makeRandStages degreesToTest) |> List.append
                // (makeRandCoComp degreesToTest) |> List.append
                // (makeRandSymmetric degreesToTest) |> List.append
                 //(makeRandStages999 degreesToTest) |> List.append
                 // (makeRandStages900 degreesToTest) |> List.append
                 // (makeRandSwitches900 degreesToTest)
                 None


        let randy = RngGen.createLcg seed |> Rando.fromRngGen
        let nextRnGen(randy:IRando) =
            RngGen.createLcg (RandomSeed.fromInt randy.NextPositiveInt)

        None
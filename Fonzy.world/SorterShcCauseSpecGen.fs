namespace global
open System

module SorterShcCauseSpecGen = 
    
    let makeRunBatchSeq (seed:RandomSeed)
                        (outputDir:FilePath) = 

        let degreesToTest = 
            [ 8; 10; 12; 14; 16; 18; 20; 22; 24;] //14; 16; 22; 24;]
            //   [ 14; 16; 18; 24;]
             |> List.map (Degree.fromInt)


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
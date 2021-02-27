namespace global
open System

module TestData =
    module CauseSpec =
        let seed = 1234
        let rnGen = RngGen.createLcg seed
        let randy = Rando.fromRngGen rnGen
        let nextRnGen() =
            RngGen.createLcg randy.NextPositiveInt

        module IntDist =
            let arrayName = "arrayName"
            let arrayName2d = "arrayName2d"
            let arrayCount = 103


            let intDistType = IntDistType.Uniform (UniformIntegerDistParams.zeroCentered 5)
            let rndUniform = CauseSpecRandGen.intArray 
                                                intDistType arrayCount 
                                                (nextRnGen()) arrayName
            
            let int2dDistType = Int2dDistType.Uniform (UniformInt2dDistParams.square 5)
            let rnd2dUniform = CauseSpecRandGen.int2dArray 
                                        int2dDistType arrayCount 
                                        (nextRnGen()) arrayName2d

        module Sorter =
            let count = 10
            let rndSortersName = "rndSortersName"
            let intDistType = IntDistType.Normal (NormalIntegerDistParams.zeroCentered 1.0)
            let degree = Degree.fromInt 8
            let sorterLength = SorterLength.degreeTo999StageCount degree
            let switchFreq = SwitchFrequency.max
            let sorterCount = SorterCount.fromInt 10

            let rand1 = CauseSpecSorters.rndGen degree sorterLength 
                             switchFreq sorterCount (nextRnGen()) rndSortersName
            let rand2 = CauseSpecSorters.rndGen degree sorterLength 
                             switchFreq sorterCount (nextRnGen()) rndSortersName
            let rand3 = CauseSpecSorters.rndGen degree sorterLength 
                             switchFreq sorterCount (nextRnGen()) rndSortersName


    module WorldAction =
        module IntDist =
            let causeRndUniform = Causes.fromCauseSpec CauseSpec.IntDist.rndUniform
                                    |> Result.ExtractOrThrow
            let randomUniform = WorldAction.create World.empty causeRndUniform

        module SorterGen = 
            let cause1 = Causes.fromCauseSpec CauseSpec.Sorter.rand1 
                            |> Result.ExtractOrThrow
            let rand1 = WorldAction.create World.empty cause1


    //let getId (job:Job) = 
    //    match job with
    //    | GetWorld w -> w.id
    //    | MakeWorld wa -> wa.childId

    //let getParentId (job:Job) = 
    //    match job with
    //    | GetWorld w -> w.parentId
    //    | MakeWorld wa -> Some wa.parentWorld.id

    //let getCause (job:Job) = 
    //    match job with
    //    | GetWorld w -> w.cause
    //    | MakeWorld wa -> wa.cause

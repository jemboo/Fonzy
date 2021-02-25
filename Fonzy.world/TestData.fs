namespace global
open System

module TestData =
    module Causes =
        let seed = 1234
        let rnGen = RngGen.createLcg seed
        let genArrayName = "genA"
        let arrayCount = 103
        let id = Guid.NewGuid()


        let intDistType = IntDistType.Uniform (UniformIntegerDistParams.zeroCentered 5)
        let causeSpecRandGenIntArray = CauseSpecRandGen.intArray 
                                            intDistType arrayCount 
                                            rnGen genArrayName
            

        let lattice2dDistType = Int2dDistType.Uniform (UniformInt2dDistParams.square 5)
        let csLl2dGen = CauseSpecRandGen.int2dArray 
                                    lattice2dDistType arrayCount 
                                    rnGen genArrayName

    module WorldActions =
        let count = 10
        let intArrayName = "intArrayName"
        let rndSortersName = "rndSortersName"
        let intDistType = IntDistType.Normal (NormalIntegerDistParams.zeroCentered 1.0)
        let rngGen = RngGen.createLcg 123
        let degree = Degree.fromInt 8
        let sorterLength = SorterLength.degreeTo999StageCount degree
        let switchFreq = SwitchFrequency.max
        let sorterCount = SorterCount.fromInt 10

        let causeSpecIntDist = CauseSpecRandGen.intArray intDistType count rngGen intArrayName
        let causeIntDist = Causes.fromCauseSpec causeSpecIntDist |> Result.ExtractOrThrow
        let worldActionGenIntDist = WorldAction.create World.empty causeIntDist

        let causeSpecRndSorters = CauseSpecSorters.rndGen degree sorterLength 
                                          switchFreq sorterCount rngGen rndSortersName
        let causeRndSorters = Causes.fromCauseSpec causeSpecRndSorters |> Result.ExtractOrThrow
        let worldActionRandGenSorters = WorldAction.create World.empty causeRndSorters





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

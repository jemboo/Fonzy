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
                                            rnGen genArrayName id
            

        let lattice2dDistType = Lattice2dDistType.Uniform (UniformLattice2dDistParams.square 5)
        let csLl2dGen = CauseSpecRandGen.latticeLoc2dArray 
                                    lattice2dDistType arrayCount 
                                    rnGen genArrayName id




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

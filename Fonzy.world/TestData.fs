namespace global
open System

module TestData =
    let seed = 1234
    let degree = Degree.fromInt 8
    let rnGen = RngGen.createLcg seed
    let randy = Rando.fromRngGen rnGen
    let nextRnGen() =
        RngGen.createLcg randy.NextPositiveInt

    module SortableSet =
        let sortableCount = SortableCount.fromInt 5
        let sortableCount2 = SortableCount.fromInt 6
        let sorterSetRndBits = SortableSetGenerated.rndBits degree sortableCount rnGen
        let sorterSetRndBits2 = SortableSetGenerated.rndBits degree sortableCount2 rnGen

    module CauseSpec =


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

    module World = 
    
        let world1Id = Guid.Parse "00000000-0000-0000-0000-000000000881"
        let world2Id = Guid.Parse "00000000-0000-0000-0000-000000000882"
        let world1ParentId = Guid.Parse "00000000-0000-0000-0000-000000000771"
        let world2ParentId = Guid.Parse "00000000-0000-0000-0000-000000000772"

        let map1 = [("key11","val11"); ("key12","val12"); ("key13","val13"); ("key14","val14")]
                   |> Map.ofList

        let map2 = [("key21","val21"); ("key22","val22"); ("key23","val23"); ("key24","val24")]
                   |> Map.ofList

        let enviro1 = Enviro.ObjectMap map1
        let enviro2 = Enviro.ObjectMap map2

        let world1 = World.create world1ParentId Causes.noOp enviro1
        let world2 = World.create world2ParentId Causes.noOp enviro2


    module WorldMerge =
        let world1Name = "world1"
        let world2Name = "world2"
        let mergedWorldId = Guid.Parse "00000000-0000-0000-0000-000000000991"

        let sourceWorldsNameMap = [(world1Name, World.world1); 
                                   (world2Name, World.world2);]
                                            |> Map.ofList

        let mm1 = {MergeMapItem.sourceMapName="world1"; 
                   MergeMapItem.sourceMapKey="key11"; 
                   outputKey="key11"}

        let mm2 = {MergeMapItem.sourceMapName="world2"; 
                   MergeMapItem.sourceMapKey="key22"; 
                   outputKey="key22"}

        let mapM = [("keyM1","valM1");] |> Map.ofList
        let enviroM = Enviro.ObjectMap mapM
        let mergeMapItems = [mm1;mm2]
        





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

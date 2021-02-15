namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CausesFixture () =

    [<TestMethod>]
    member this.CauseFromCauseSpecIntArrayRandGen () =
        let genArrayName = "genA"
        let arrayCount = 103
        let randy = RngGen.createLcg 22
        let id = Guid.NewGuid()
        let intDistType = IntDistType.Uniform (UniformIntegerDistParams.zeroCentered 5)
        let csIntGen = CauseSpecRandGen.intArray intDistType arrayCount randy genArrayName id

        let cause = Causes.fromCauseSpec csIntGen |> Result.ExtractOrThrow
        let env = Enviro.Empty
        let newEnv = cause.op env |> Result.ExtractOrThrow
        let generated = newEnv |> Enviro.toMap |> ResultMap.read genArrayName |> Result.ExtractOrThrow
        let intDist = generated |> IntDistDto.fromJson |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, arrayCount)

    [<TestMethod>]
    member this.CauseFromCauseSpecLatticeLoc2dArrayRandGen () =
        let genArrayName = "genA"
        let arrayCount = 103
        let randy = RngGen.createLcg 22
        let id = Guid.NewGuid()
        let lattice2dDistType = Lattice2dDistType.Uniform (UniformLattice2dDistParams.square 5)
        let csLl2dGen = CauseSpecRandGen.latticeLoc2dArray lattice2dDistType arrayCount randy genArrayName id

        let cause = Causes.fromCauseSpec csLl2dGen |> Result.ExtractOrThrow
        let env = Enviro.Empty
        let newEnv = cause.op env |> Result.ExtractOrThrow
        let generated = newEnv |> Enviro.toMap |> ResultMap.read genArrayName |> Result.ExtractOrThrow
        let intDist = generated |> Lattice2dDistDto.fromJson |> Result.ExtractOrThrow
        Assert.AreEqual(intDist.vals.Length, arrayCount)
namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type StoHillClimbFixture () =

    [<TestMethod>]
    member this.Annealer_makeConst() =
        let constTemp = Temp.fromFloat 1.0
        let oldFitness = Energy.fromFloat 2.0
        let newFitnessBetter = Energy.fromFloat 1.0
        let newFitnessWorse = Energy.fromFloat 3.0
        let caster = fun () -> 0.5
        let annFC = Annealer.makeConst constTemp
        let step = StepNumber.fromInt 0
        let resT = annFC oldFitness newFitnessBetter caster step
        Assert.IsTrue(resT)
        let resF = annFC oldFitness newFitnessWorse caster step
        Assert.IsFalse(resF)


    [<TestMethod>]
    member this.Annealer_makeExp() =
        let constTemp = Temp.fromFloat 1.0
        let oldFitness = Energy.fromFloat 2.0
        let newFitnessBetter = Energy.fromFloat 1.0
        let newFitnessWorse = Energy.fromFloat 3.0
        let decaySteps = 20.0
        let caster = fun () -> 0.5
        let annFC = Annealer.makeExp constTemp decaySteps
        let step = StepNumber.fromInt 20
        let resT = annFC oldFitness newFitnessBetter caster step
        Assert.IsTrue(resT)
        let resF = annFC oldFitness newFitnessWorse caster step
        Assert.IsFalse(resF)


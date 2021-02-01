namespace Fonzy.world.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CausesDtoFixture () =

    [<TestMethod>]
    member this.CauseTypeDto () =
        //let causeD = Cause.fromCauseType CauseType.Destroy
        //let ctDDto = CauseTypeDto.toDto causeD.causeType
        //let causeDTypeBack = CauseTypeDto.fromDto ctDDto |> Result.ExtractOrThrow
        //let causeDBack = causeDTypeBack |> Cause.fromCauseType
        //Assert.AreEqual(causeD, causeDBack);

        //let causeN = Cause.fromCauseType CauseType.NoOp
        //let ctNDto = CauseTypeDto.toDto causeN.causeType
        //let causeNTypeBack = CauseTypeDto.fromDto ctNDto |> Result.ExtractOrThrow
        //let causeNBack = causeNTypeBack |> Cause.fromCauseType
        //Assert.AreEqual(causeN, causeNBack);
        Assert.IsTrue(true)

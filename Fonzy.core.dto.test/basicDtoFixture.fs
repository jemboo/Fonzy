namespace Fonzy.core.dto.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json

[<TestClass>]
type BasicDtoFixture () =

    [<TestMethod>]
    member this.boolOptDto() =
        let bopT = Some true
        let bopTc = bopT |> BasicDto.toCereal
        let bopTb = bopTc |> BasicDto.fromCereal |> Result.ExtractOrThrow
        Assert.AreEqual(1, 1)
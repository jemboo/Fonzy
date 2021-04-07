namespace Fonzy.core.dto.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json

[<TestClass>]
type CommonTypesDtoFixture () =

    [<TestMethod>]
    member this.SerializeMap() =
        let kvps = [|("a","a"); ("b", "b"); ("c", "c"); ("d", "d") |]
                    |> Map.ofArray
        let json = JsonConvert.SerializeObject(kvps)
        let kvpsB = JsonConvert.DeserializeObject<Map<string,string>>(json)
        Assert.AreEqual(kvps, kvpsB)


    //[<TestMethod>]
    //member this.SerializeMutationType() =
    //    let mt = SorterMutationType.Stage (MutationRate.fromFloat 0.1)
    //    let json = SorterMutationTypeDto.toJson mt
    //    let mtBack = SorterMutationTypeDto.fromJson json |> Result.ExtractOrThrow
    //    Assert.AreEqual(mt, mtBack)
namespace Fonzy.core.dto.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json

[<TestClass>]
type AnnealerDtoFixture () =

    [<TestMethod>]
    member this.annealerSpecDto() =
        let spec = annealerSpec.Constant (Temp.fromFloat 1.0)
        let specDto = spec |> AnnealerSpecDto.toDto
        let specBack = specDto |> AnnealerSpecDto.fromDto
                        |> Result.ExtractOrThrow
        Assert.AreEqual(spec, specBack)

        let spec1 = annealerSpec.Exp ((Temp.fromFloat 1.0), 2.2);
        let specDto1 = spec1 |> AnnealerSpecDto.toDto
        let specBack1 = specDto1 |> AnnealerSpecDto.fromDto
                        |> Result.ExtractOrThrow
        Assert.AreEqual(spec1, specBack1)


    //[<TestMethod>]
    //member this.SerializeMutationType() =
    //    let mt = SorterMutationType.Stage (MutationRate.fromFloat 0.1)
    //    let json = SorterMutationTypeDto.toJson mt
    //    let mtBack = SorterMutationTypeDto.fromJson json |> Result.ExtractOrThrow
    //    Assert.AreEqual(mt, mtBack)
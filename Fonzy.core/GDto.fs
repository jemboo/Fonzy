namespace global
open System

type SorterPoolEnviromentDto = {cat:string; value:string}
module SorterPoolEnviromentDto =
    let toDto (env:SorterPoolEnvironment) =
         match env with
         | SorterPoolEnvironment.Bag n -> {cat="Bag"; value = Json.serialize n}
         | SorterPoolEnvironment.Torus n -> {cat="Torus"; value = Json.serialize n}


    let fromDto (eDto:SorterPoolEnviromentDto) =
        if eDto.cat = "Bag" then
            result {
                let! b = Json.deserialize<int> eDto.value
                return SorterPoolEnvironment.Bag b
            }
        else if eDto.cat = "Torus" then
            result {
                let! b = Json.deserialize<int> eDto.value
                return SorterPoolEnvironment.Torus b
            }
        else sprintf "cat: %s for SorterPoolEnviromentDto not found"
                      eDto.cat |> Error


type EnvironmentDto = {cat:string; value:string}
module EnvironmentDto =
    let toDto (env:Enviroment) =
        match env with
        | Enviroment.A n -> {cat="A"; value = Json.serialize n}
        | Enviroment.B n -> {cat="B"; value = Json.serialize n}
        | Enviroment.S n -> {cat="S"; value = n |> SorterPoolEnviromentDto.toDto |> Json.serialize}
        | Enviroment.Empty -> {cat="Empty"; value = Json.serialize None}

    let fromDto (eDto:EnvironmentDto) =
        if eDto.cat = "Empty" then
            result {
                return Enviroment.Empty
            }
        else if eDto.cat = "S" then
            result {
                let! b = Json.deserialize<SorterPoolEnviromentDto> eDto.value
                let! q = b |> SorterPoolEnviromentDto.fromDto
                return Enviroment.S q
            }
        else if eDto.cat = "A" then
            result {
                let! b = Json.deserialize<int> eDto.value
                return Enviroment.A b
            }
        else if eDto.cat = "B" then
            result {
                let! b = Json.deserialize<float> eDto.value
                return Enviroment.B b
            }
        else sprintf "cat: %s for EnvironmentDto not found"
                      eDto.cat |> Error


type TestCauseTypeDto = {cat:string; value:string}
module TestCauseTypeDto =
                      
    let toDto (ct:TestCauseType) =
        match ct with
        | TestCauseType.AddFloat n -> {cat="AddFloat"; value = Json.serialize n}
        | TestCauseType.AddInt n -> {cat="AddInt"; value = Json.serialize n}
        | TestCauseType.CreateFloat n -> {cat="CreateFloat"; value = Json.serialize n}
        | TestCauseType.CreateInt n -> {cat="CreateInt"; value = Json.serialize n}
                      
    let fromDto (eDto:TestCauseTypeDto) =
        if eDto.cat = "AddFloat" then
            result {
                let! b = Json.deserialize<float> eDto.value
                return TestCauseType.AddFloat b
            }
        else if eDto.cat = "AddInt" then
            result {
                let! b = Json.deserialize<int> eDto.value
                return TestCauseType.AddInt b
            }
        else if eDto.cat = "CreateFloat" then
            result {
                let! b = Json.deserialize<float> eDto.value
                return TestCauseType.CreateFloat b
            }
        else if eDto.cat = "CreateInt" then
            result {
                let! b = Json.deserialize<int> eDto.value
                return TestCauseType.CreateInt b
            }
        else sprintf "cat: %s for TestCauseTypeDto not found"
                    eDto.cat |> Error


type CauseTypeDto = {cat:string; value:string}
module CauseTypeDto =

    let toDto (ct:CauseType) =
        match ct with
        | CauseType.Test tc -> {cat="Test"; value = tc |> TestCauseTypeDto.toDto |> Json.serialize}
        | CauseType.Destroy -> {cat="Destroy"; value = Json.serialize None}
        | CauseType.NoOp -> {cat="NoOp"; value = Json.serialize None}

    let fromDto (eDto:CauseTypeDto) =
        if eDto.cat = "Test" then
            result {
                let! dto = Json.deserialize<TestCauseTypeDto> eDto.value
                let! tct = TestCauseTypeDto.fromDto dto
                return CauseType.Test tct
            }
        else if eDto.cat = "Destroy" then
            result {
                return CauseType.Destroy
            }
        else if eDto.cat = "NoOp" then
            result {
                return CauseType.NoOp
            }
        else sprintf "cat: %s for CauseTypeDto not found"
                      eDto.cat |> Error




type WorldDto = {id:Guid; parentId:Guid option; causeTypeDto:CauseTypeDto; environmentDto:EnvironmentDto}
module WorldDto = 
    let create (id:Guid) (parentId:Guid option) (causeTypeDto:CauseTypeDto) (environmentDto:EnvironmentDto) =
               {
                    id=id; 
                    parentId=parentId; 
                    causeTypeDto=causeTypeDto; 
                    environmentDto=environmentDto
                }

    let toDto (w:World) =
         {
             id=w.id; 
             parentId=w.parentId; 
             causeTypeDto=w.cause.causeType |> CauseTypeDto.toDto; 
             environmentDto=w.enviroment |> EnvironmentDto.toDto; 
         }

    let fromDto (worldDto:WorldDto) =
            result {
                let! enviroment =  EnvironmentDto.fromDto worldDto.environmentDto
                let! causeType =  CauseTypeDto.fromDto worldDto.causeTypeDto
                let cause = Cause.fromCauseType causeType
                return World.create worldDto.id worldDto.parentId cause enviroment
            }
            
            
            
type WorldActionDto = {parentWorldDto:WorldDto; childId:Guid; causeTypeDto:CauseTypeDto;}
module WorldActionDto = 
    let create (childId:Guid) (parentWorldDto:WorldDto) (causeTypeDto:CauseTypeDto) =
                {
                    childId=childId; 
                    parentWorldDto=parentWorldDto; 
                    causeTypeDto=causeTypeDto;
                }
            
    let toDto (parentWorld:World) (causeType:CauseType) (childId:Guid) =
            {
                childId = childId; 
                parentWorldDto = parentWorld |> WorldDto.toDto; 
                causeTypeDto=causeType |> CauseTypeDto.toDto;
            }
            
    let fromDto (worldActionDto:WorldActionDto) =
            result {
                let! parentWorld =  WorldDto.fromDto worldActionDto.parentWorldDto
                let! causeType =  CauseTypeDto.fromDto worldActionDto.causeTypeDto
                let cause = Cause.fromCauseType causeType
                return! World.createFromParent worldActionDto.childId parentWorld cause
             }

type JobFile =
    | ReadWorld of string
    | MakeWorld of string

module JobFile =
    let toWorld (jobFile:JobFile) = 
        match jobFile with
        | ReadWorld json -> 
            result {
                let! dto = json |> Json.deserialize<WorldDto>
                return! dto |> WorldDto.fromDto
            }
        | MakeWorld json -> 
            result {
                let! dto = json |> Json.deserialize<WorldActionDto>
                return! dto |> WorldActionDto.fromDto
            }


type JobFileDto = {cat:string; value:string}
module JobFileDto =
    let toDto (jf:JobFile) =
         match jf with
         | JobFile.ReadWorld n -> {cat="ReadWorld"; value = n}
         | JobFile.MakeWorld n -> {cat="MakeWorld"; value = n}


    let fromDto (eDto:JobFileDto) =
        if eDto.cat = "ReadWorld" then
            result {
                return JobFile.ReadWorld eDto.value
            }
        else if eDto.cat = "MakeWorld" then
            result {
                return JobFile.MakeWorld eDto.value
            }
        else sprintf "cat: %s for JobFileDto not found"
                      eDto.cat |> Error
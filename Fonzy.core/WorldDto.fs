namespace global

open System

type EnvironmentDto = EnvironmentDto of Map<string, obj>
module EnvironmentDto =
    let create (kvps: (string * obj) list) = 
        kvps |> Map.ofList |> EnvironmentDto
    let value (EnvironmentDto causeTypeDto) = causeTypeDto

    let toDto (env:Enviroment) =
         match env with
         | Enviroment.A n -> [("A", n :> obj)] |> create
         | Enviroment.B n -> [("B", n :> obj)] |> create
         | Enviroment.Empty -> [("Empty", None :> obj)] |> create

    let fromDto (eDto:EnvironmentDto) =
        let v = (value eDto)
        if v.ContainsKey "A" then
            CollectionUtils.getTypeFromMap<int64> "A" v |> Result.map int 
                                                        |> Result.map Enviroment.A
        else if v.ContainsKey "B" then
            CollectionUtils.getTypeFromMap<int64> "B" v |> Result.map float 
                                                        |> Result.map Enviroment.B
        else if v.ContainsKey "Empty" then
            Enviroment.Empty |> Ok

        else sprintf "keys for EnvironmentDto not found" |> Error


type CauseTypeDto = CauseTypeDto of Map<string, obj>
module CauseTypeDto =
    let create (kvps: (string * obj) list) = 
        kvps |> Map.ofList |> CauseTypeDto
    let value (CauseTypeDto causeTypeDto) = causeTypeDto

    let toDto (ct:CauseType) =
         match ct with
         | CauseType.AddFloat n -> [("AddFloat", n :> obj)] |> create
         | CauseType.AddInt n -> [("AddInt", n :> obj)] |> create
         | CauseType.CreateFloat n -> [("CreateFloat", n :> obj)] |> create
         | CauseType.CreateInt n -> [("CreateInt", n :> obj)] |> create
         | CauseType.Destroy  -> [("Destroy", None :> obj)]  |> create
         | CauseType.NoOp ->  [("NoOp", None :> obj)]  |> create

    let fromDto (eDto:CauseTypeDto) =
        let v = (value eDto)
        if v.ContainsKey "AddFloat" then
            CollectionUtils.getTypeFromMap<float> "AddFloat" 
                    v |> Result.map float
                      |> Result.map CauseType.AddFloat

        else if v.ContainsKey "AddInt" then
             CollectionUtils.getTypeFromMap<int64> "AddInt" 
                    v |> Result.map int
                      |> Result.map CauseType.AddInt

        else if v.ContainsKey "CreateFloat" then
            CollectionUtils.getTypeFromMap<float> "CreateFloat" 
                    v |> Result.map float
                    |> Result.map CauseType.CreateFloat

        else if v.ContainsKey "CreateInt" then
            CollectionUtils.getTypeFromMap<int64> "CreateInt" 
                    v |> Result.map int
                        |> Result.map CauseType.CreateInt

        else if v.ContainsKey "Destroy" then
            CauseType.Destroy |> Ok

        else if  v.ContainsKey "NoOp" then
            CauseType.NoOp |> Ok

        else sprintf "keys for CauseTypeDto not found" |> Error


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



//type WorldDto = private {id:Guid; parentId:Guid option; causeTypeDto:CauseTypeDto; environmentDto:EnvironmentDto}
//module WorldDto = 
//    let create (id:Guid) (parentId:Guid option) (causeTypeDto:CauseTypeDto) (environmentDto:EnvironmentDto) =
//               {
//                    id=id; 
//                    parentId=parentId; 
//                    causeTypeDto=causeTypeDto; 
//                    environmentDto=environmentDto
//                }

//    let toDto (w:World) =
//         {
//             id=w.id; 
//             parentId=w.parentId; 
//             causeTypeDto=w.cause.causeType |> CauseTypeDto.toDto; 
//             environmentDto=w.enviroment |> EnvironmentDto.toDto; 
//         }

//    let fromDto (worldDto:WorldDto) =
//            result {
//                let! enviroment =  EnvironmentDto.fromDto worldDto.environmentDto
//                let! causeType =  CauseTypeDto.fromDto worldDto.causeTypeDto
//                let cause = Cause.fromCauseType causeType
//                return World.create worldDto.id worldDto.parentId cause enviroment
//            }
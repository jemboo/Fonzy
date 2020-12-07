namespace global
type WorldsDto = {name:string}

module WorldsDto = 
    let q = None




//type WorldDto = {id:Guid; parentId:Guid option; causeTypeDto:CauseTypeDto; environmentDto:EnviroDto}
//module WorldDto = 
//    let create (id:Guid) (parentId:Guid option) (causeTypeDto:CauseTypeDto) (environmentDto:EnviroDto) =
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
//             environmentDto=w.enviroment |> EnviroDto.toDto; 
//         }

//    let fromDto (worldDto:WorldDto) =
//            result {
//                let! enviroment =  EnviroDto.fromDto worldDto.environmentDto
//                let! causeType =  CauseTypeDto.fromDto worldDto.causeTypeDto
//                let cause = Cause.fromCauseType causeType
//                return World.create worldDto.id worldDto.parentId cause enviroment
//            }
            
            
            
//type WorldActionDto = {childId:Guid; parentWorldDto:WorldDto;  causeTypeDto:CauseTypeDto;}
//module WorldActionDto = 
//    let create (childId:Guid) (parentWorldDto:WorldDto) (causeTypeDto:CauseTypeDto) =
//                {
//                    childId=childId; 
//                    parentWorldDto=parentWorldDto; 
//                    causeTypeDto=causeTypeDto;
//                }
            
//    let toDto (worldAction:WorldAction) =
//            {
//                childId = worldAction.childId; 
//                parentWorldDto = worldAction.parentWorld |> WorldDto.toDto; 
//                causeTypeDto = worldAction.cause.causeType |> CauseTypeDto.toDto;
//            }
            
//    let fromDto (worldActionDto:WorldActionDto) =
//            result {
//                let! parentWorld =  WorldDto.fromDto worldActionDto.parentWorldDto
//                let! causeType =  CauseTypeDto.fromDto worldActionDto.causeTypeDto
//                let cause = Cause.fromCauseType causeType
//                return WorldAction.create (worldActionDto.childId) parentWorld cause
//             }


namespace global
open System

type GridLocationDto = 
    {
        x:int;
        y:int;
    }

//module GridLocationDto = 
//    let fromDto (dto:GridLocationDto) =
//        result {
//            let x = dto.x
//            let y = dto.y
//            return {
//                x=x;
//                y=y;
//            }
//        }

//    let toDto (gridLocation:GrLoc2d) =
//        {
//            x =  gridLocation.x
//            y = gridLocation.y
//        }



//type PoolOfGridLocationsDto = 
//    {
//        id:Guid;
//        locMap:Map<Guid, GrLoc2d>
//    }

//module PoolOfGridLocationsDto = 
//    let fromDto (dto:PoolOfGridLocationsDto) =
//        result {
//            return {
//                PoolOfGridLocations.id= dto.id;
//                locPool = dto.locMap
//            }
//        }

//    let toDto (poolOfGridLocations:PoolOfGridLocations) =
//        {
//            PoolOfGridLocationsDto.id = poolOfGridLocations.id
//            PoolOfGridLocationsDto.locMap = poolOfGridLocations.locPool
//        }
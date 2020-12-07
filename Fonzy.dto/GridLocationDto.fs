namespace global
open System

type GridLocationDto = 
    {
        id:Guid;
        x:int;
        y:int;
    }

module GridLocationDto = 
    let fromDto (dto:GridLocationDto) =
        result {
            let id = dto.id
            let x = dto.x
            let y = dto.y
            return {
                GridLocation.id=id;
                x=x;
                y=y;
            }
        }

    let toDto (gridLocation:GridLocation) =
        {
            GridLocationDto.id = gridLocation.id
            x =  gridLocation.x
            y = gridLocation.y
        }

type PoolOfGridLocationsDto = 
    {
        id:Guid;
        locPool:GridLocationDto[]
    }

module PoolOfGridLocationsDto = 
    let fromDto (dto:PoolOfGridLocationsDto) =
        result {
            let id = dto.id
            let! locList = dto.locPool
                        |> Array.map(GridLocationDto.fromDto)
                        |> Array.toList |> Result.sequence
            let locPool = locList |> List.map(fun gl-> (gl.id, gl))
                                  |> Map.ofList
            return {
                PoolOfGridLocations.id=id;
                locPool = locPool
            }
        }

    let toDto (poolOfGridLocations:PoolOfGridLocations) =
        {
            PoolOfGridLocationsDto.id = poolOfGridLocations.id
            PoolOfGridLocationsDto.locPool = 
                poolOfGridLocations.locPool
                    |> Map.toArray
                    |> Array.map(fun tup-> snd tup)
                    |> Array.map(GridLocationDto.toDto)
        }
namespace global
open System


module SwitchDto =
    let fromDto dex = Switch.switchMap.[dex] |> Ok
    let toDto (switch:Switch) =
        (switch.hi * (switch.hi + 1)) / 2 + switch.low
     

type SorterDto = {degree:int; switches:int[]}
module SorterDto =
    let fromDto (dto:SorterDto) =
        result {
            let! degree = Degree.create "" dto.degree
            let! switches = dto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                        |> Array.toList
                                        |> Result.sequence
            return Sorter.create degree switches
        }
    let fromJson (cereal:string) =
        result {
            let! sorterDto = cereal |> Json.deserialize<SorterDto>
            return! fromDto sorterDto
        }
    let toDto (sorter:Sorter) =
        {
            SorterDto.degree = (Degree.value sorter.degree); 
            switches = sorter.switches |> Array.map(fun sw -> SwitchDto.toDto sw)
        }
    let toJson (sorter:Sorter) =
        sorter |> toDto |> Json.serialize


type SorterSetDto = {degree:int; sorterDtos:SorterDto[]}
module SorterSetDto =
    let fromDto (dto:SorterSetDto) =
        result {
            let! degree = dto.degree |> Degree.create ""
            let! sorters = dto.sorterDtos |> Array.map(SorterDto.fromDto)
                                          |> Array.toList
                                          |> Result.sequence
            let! sorterCount = sorters.Length |> SorterCount.create ""
            return {
                  SorterSet.degree=degree;
                  SorterSet.sorterCount = sorterCount; 
                  SorterSet.sorters=sorters |> List.toArray
                }
        }
    let fromJson (cereal:string) =
        result {
            let! sorterDto = cereal |> Json.deserialize<SorterSetDto>
            return! fromDto sorterDto
        }
    let toDto (sorterSet:SorterSet) =
        {
            SorterSetDto.sorterDtos = sorterSet.sorters 
                                        |> Array.map(SorterDto.toDto)
            SorterSetDto.degree = (Degree.value sorterSet.degree)
        }
    let toJson (sorters:SorterSet) =
        sorters |> toDto |> Json.serialize


type SwitchUsesDto = {switchCount:int; weights:int[]}
module SwitchUsesDto =
    let fromDto (dto:SwitchUsesDto) =
        result {
            let! switchCount = SwitchCount.create "" dto.switchCount
            return! SwitchUses.create switchCount dto.weights
        }

    let fromJson (cereal:string) =
        result {
            let! sorterDto = cereal |> Json.deserialize<SwitchUsesDto>
            return! fromDto sorterDto
        }

    let toDto (switchUses:SwitchUses) =
        {SwitchUsesDto.switchCount= (SwitchUses.switchCount switchUses); 
         weights = (SwitchUses.getWeights switchUses)}

    let toJson (switchUses:SwitchUses) =
        switchUses |> toDto |> Json.serialize

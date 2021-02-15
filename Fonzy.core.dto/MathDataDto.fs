namespace global
open System


type IntDistDto = {intDistTypeDto:IntDistTypeDto; values:int[]}
module IntDistDto =
    let fromDto (dto:IntDistDto) =
        result {
            let! idt = IntDistTypeDto.fromDto dto.intDistTypeDto
            return {IntDist.intDistType = idt; 
                    IntDist.vals = dto.values}
        }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<IntDistDto> js
            return! fromDto dto
        }

    let toDto (intD:IntDist) =
        {
            IntDistDto.intDistTypeDto = intD.intDistType 
                                                 |> IntDistTypeDto.toDto;
            IntDistDto.values = intD.vals
        }

    let toJson (idt:IntDist) =
        idt |> toDto |> Json.serialize


type Lattice2dDistDto = {lattice2dDistTypeDto:Lattice2dDistTypeDto; 
                                 values:LatticeLoc2d[]}
module Lattice2dDistDto =
    let fromDto (dto:Lattice2dDistDto) =
        result {
            let! ldt = Lattice2dDistTypeDto.fromDto dto.lattice2dDistTypeDto
            return {Lattice2dDist.lattice2dDistType = ldt; 
                    Lattice2dDist.vals = dto.values}
        }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<Lattice2dDistDto> js
            return! fromDto dto
        }

    let toDto (l2dD:Lattice2dDist) =
        {
            Lattice2dDistDto.lattice2dDistTypeDto = l2dD.lattice2dDistType 
                                                        |> Lattice2dDistTypeDto.toDto;
            Lattice2dDistDto.values = l2dD.vals;
         }

    let toJson (idt:Lattice2dDist) =
        idt |> toDto |> Json.serialize
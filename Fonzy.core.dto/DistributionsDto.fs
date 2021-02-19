namespace global
open System


type IntDistTypeDto = {cat:string; value:string}
module IntDistTypeDto =
    let fromDto (dto:IntDistTypeDto) =
        if dto.cat = "Uniform" then
            result {
                let! b = Json.deserialize<UniformIntegerDistParams> dto.value
                return IntDistType.Uniform b
            }
        else if dto.cat = "Normal" then
            result {
                let! b = Json.deserialize<NormalIntegerDistParams> dto.value
                return IntDistType.Normal b
            }
        else sprintf "cat: %s for IntDistTypeDto not found"
                        dto.cat |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<IntDistTypeDto> jstr
            return! fromDto dto
        }

    let toDto (idt:IntDistType) =
        match idt with
        | IntDistType.Uniform up -> {IntDistTypeDto.cat="Uniform"; 
                                    value=Json.serialize up}
        | IntDistType.Normal np -> {IntDistTypeDto.cat="Normal"; 
                                    value=Json.serialize np}

    let toJson (idt:IntDistType) =
        idt |> toDto |> Json.serialize

            
type Lattice2dDistTypeDto = {cat:string; value:string}
module Lattice2dDistTypeDto =
    let fromDto (dto:Lattice2dDistTypeDto) =
        if dto.cat = "Uniform" then
            result {
                let! b = Json.deserialize<UniformLattice2dDistParams> dto.value
                return Lattice2dDistType.Uniform b
            }
        else if dto.cat = "Normal" then
            result {
                let! b = Json.deserialize<NormalLattice2dDistParams> dto.value
                return Lattice2dDistType.Normal b
            }
        else sprintf "cat: %s for Lattice2dDistTypeDto not found"
                        dto.cat |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<Lattice2dDistTypeDto> jstr
            return! fromDto dto
        }

    let toDto (idt:Lattice2dDistType) =
        match idt with
        | Lattice2dDistType.Uniform up -> {Lattice2dDistTypeDto.cat="Uniform"; 
                                            value = Json.serialize up}
        | Lattice2dDistType.Normal np -> {Lattice2dDistTypeDto.cat="Normal";
                                            value = Json.serialize np}

    let toJson (idt:Lattice2dDistType) =
        idt |> toDto |> Json.serialize



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
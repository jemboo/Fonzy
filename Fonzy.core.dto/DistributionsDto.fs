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

            
type Int2dDistTypeDto = {cat:string; value:string}
module Int2dDistTypeDto =
    let fromDto (dto:Int2dDistTypeDto) =
        if dto.cat = "Uniform" then
            result {
                let! b = Json.deserialize<UniformInt2dDistParams> dto.value
                return Int2dDistType.Uniform b
            }
        else if dto.cat = "Normal" then
            result {
                let! b = Json.deserialize<NormalInt2dDistParams> dto.value
                return Int2dDistType.Normal b
            }
        else sprintf "cat: %s for Int2dDistTypeDto not found"
                        dto.cat |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<Int2dDistTypeDto> jstr
            return! fromDto dto
        }

    let toDto (idt:Int2dDistType) =
        match idt with
        | Int2dDistType.Uniform up -> {Int2dDistTypeDto.cat="Uniform"; 
                                            value = Json.serialize up}
        | Int2dDistType.Normal np -> {Int2dDistTypeDto.cat="Normal";
                                            value = Json.serialize np}

    let toJson (idt:Int2dDistType) =
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


type Int2dDistDto = {lattice2dDistTypeDto:Int2dDistTypeDto; 
                                 values:Int2d[]}
module Int2dDistDto =
    let fromDto (dto:Int2dDistDto) =
        result {
            let! ldt = Int2dDistTypeDto.fromDto dto.lattice2dDistTypeDto
            return {Int2dDist.lattice2dDistType = ldt; 
                    Int2dDist.vals = dto.values}
        }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<Int2dDistDto> js
            return! fromDto dto
        }

    let toDto (l2dD:Int2dDist) =
        {
            Int2dDistDto.lattice2dDistTypeDto = l2dD.lattice2dDistType 
                                                        |> Int2dDistTypeDto.toDto;
            Int2dDistDto.values = l2dD.vals;
         }

    let toJson (idt:Int2dDist) =
        idt |> toDto |> Json.serialize
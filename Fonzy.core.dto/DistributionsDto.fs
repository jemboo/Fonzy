namespace global
open System


type intDistTypeDto = {cat:string; value:string}
module IntDistTypeDto =
    let fromDto (dto:intDistTypeDto) =
        match dto.cat with
        | nameof intDistType.Uniform ->
            result {
                let! b = Json.deserialize<uniformIntegerDistParams> dto.value
                return intDistType.Uniform b
            }
        | nameof intDistType.Normal ->
            result {
                let! b = Json.deserialize<normalIntegerDistParams> dto.value
                return intDistType.Normal b
            }
        | t -> sprintf "cat: %s for IntDistTypeDto not found"
                     dto.cat |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<intDistTypeDto> jstr
            return! fromDto dto
        }

    let toDto (idt:intDistType) =
        match idt with
        | intDistType.Uniform up -> 
                        { intDistTypeDto.cat = nameof intDistType.Uniform; 
                          value=Json.serialize up}
        | intDistType.Normal np -> 
                        { intDistTypeDto.cat = nameof intDistType.Normal; 
                          value=Json.serialize np}

    let toJson (idt:intDistType) =
        idt |> toDto |> Json.serialize

            
type int2dDistTypeDto = {cat:string; value:string}
module Int2dDistTypeDto =
    let fromDto (dto:int2dDistTypeDto) =
        match dto.cat with
        | nameof Int2dDistType.Uniform -> result {
                let! b = Json.deserialize<UniformInt2dDistParams> dto.value
                return Int2dDistType.Uniform b
            }
        | nameof Int2dDistType.Normal -> 
            result {
                let! b = Json.deserialize<NormalInt2dDistParams> dto.value
                return Int2dDistType.Normal b
            }
        | t -> (sprintf "cat: %s for Int2dDistTypeDto not found"
                    dto.cat ) |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<int2dDistTypeDto> jstr
            return! fromDto dto
        }

    let toDto (idt:Int2dDistType) =
        match idt with
        | Int2dDistType.Uniform up -> {
                            int2dDistTypeDto.cat = nameof Int2dDistType.Uniform; 
                            value = Json.serialize up}
        | Int2dDistType.Normal np -> {
                            int2dDistTypeDto.cat = nameof Int2dDistType.Normal;
                            value = Json.serialize np}

    let toJson (idt:Int2dDistType) =
        idt |> toDto |> Json.serialize



type intDistDto = {intDistTypeDto:intDistTypeDto; values:int[]}
module IntDistDto =
    let fromDto (dto:intDistDto) =
        result {
            let! idt = IntDistTypeDto.fromDto dto.intDistTypeDto
            return {intDist.intDistType = idt; 
                    intDist.vals = dto.values}
        }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<intDistDto> js
            return! fromDto dto
        }

    let toDto (intD:intDist) =
        {
            intDistDto.intDistTypeDto = intD.intDistType 
                                        |> IntDistTypeDto.toDto;
            intDistDto.values = intD.vals
        }

    let toJson (idt:intDist) =
        idt |> toDto |> Json.serialize


type int2dDistDto = {lattice2dDistTypeDto:int2dDistTypeDto; 
                                 values:int2d[]}
module Int2dDistDto =
    let fromDto (dto:int2dDistDto) =
        result {
            let! ldt = Int2dDistTypeDto.fromDto dto.lattice2dDistTypeDto
            return {Int2dDist.lattice2dDistType = ldt; 
                    Int2dDist.vals = dto.values}
        }

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<int2dDistDto> js
            return! fromDto dto
        }

    let toDto (l2dD:Int2dDist) =
        {
            int2dDistDto.lattice2dDistTypeDto = l2dD.lattice2dDistType 
                                                |> Int2dDistTypeDto.toDto;
            int2dDistDto.values = l2dD.vals;
         }

    let toJson (idt:Int2dDist) =
        idt |> toDto |> Json.serialize
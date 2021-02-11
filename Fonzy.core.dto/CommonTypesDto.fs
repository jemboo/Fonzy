namespace global

type LogFileDto = {cat:string; descr:string; header:string; records:string[]}

type CustomParamsDto = {parseKey:string; prams:Map<string, string>}

type PermutationDto = private {degree:int; values:int[] }
module PermutationDto =
    let fromDto (dto:PermutationDto) =
        result {
            let! degree = Degree.create "" dto.degree
            return! Permutation.create degree dto.values
        }
    let toDto (perm:Permutation) =
        {degree= (Degree.value (Permutation.degree perm)); 
        values = Permutation.arrayValues perm}


type SorterLengthDto = {wOrT:string; value:int;}
module SorterLengthDto =
    
    let toDto (sorterLength:SorterLength) =
        match sorterLength with
        | SorterLength.Switch ct -> {wOrT="Switch"; value=(SwitchCount.value ct)}
        | SorterLength.Stage ct -> {wOrT="Stage"; value=(StageCount.value ct);}

    let toJson (sorterLength:SorterLength) =
        sorterLength |> toDto |> Json.serialize

    let fromDto (dto:SorterLengthDto) =
        let parseCat cat count =
            match cat with
            | "Switch" -> SorterLength.Switch 
                                    ((SwitchCount.create "" count)|> Result.ExtractOrThrow) |> Ok
            | "Stage" -> SorterLength.Stage 
                                    ((StageCount.create "" count)|> Result.ExtractOrThrow) |> Ok
            | _ -> Error (sprintf "no match for SorterLengthDto: %s" cat)
        result {
            return! parseCat dto.wOrT dto.value
        }


type SorterMutationTypeDto = {mType:string; rate:float}
module SorterMutationTypeDto =
    let fromDto (dto:SorterMutationTypeDto) =
        result {
                let! mr = MutationRate.create "" dto.rate
                match dto.mType with
                | "Switch" -> return SorterMutationType.Switch mr
                | "Stage" ->  return SorterMutationType.Stage mr
                | _ ->        let! res =  Error (sprintf "no match for SorterMutationType: %s" dto.mType)
                              return res
            }

    let toDto (mutationType:SorterMutationType) =
        match mutationType with
        | SorterMutationType.Switch mr -> {mType="Switch"; rate=MutationRate.value mr}
        | SorterMutationType.Stage mr -> {mType="Stage"; rate=MutationRate.value mr}

    let toJson (mutationType:SorterMutationType) =
        toDto mutationType |> Json.serialize

    let fromJson (json:string) =
        result {
            let! dto = Json.deserialize<SorterMutationTypeDto> json
            return! dto |> fromDto
        }


    type TwoCyclePermDto = private {degree:int; values:int[] }
    module TwoCyclePermDto =
        let fromDto (dto:TwoCyclePermDto) =
            result {
                let! degree = Degree.create "" dto.degree
                return! TwoCyclePerm.create degree dto.values
            }
        let toDto (tcp:TwoCyclePerm) =
            {degree= (Degree.value (TwoCyclePerm.degree tcp)); 
            values=TwoCyclePerm.arrayValues tcp;}


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

    let toDto (idt:IntDistType) =
        match idt with
        | IntDistType.Uniform up -> {IntDistTypeDto.cat="Uniform"; 
                                    value=Json.serialize up}
        | IntDistType.Normal np -> {IntDistTypeDto.cat="Normal"; 
                                    value=Json.serialize np}

            
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

    let toDto (idt:Lattice2dDistType) =
        match idt with
        | Lattice2dDistType.Uniform up -> {Lattice2dDistTypeDto.cat="Uniform"; 
                                            value = Json.serialize up}
        | Lattice2dDistType.Normal np -> {Lattice2dDistTypeDto.cat="Normal";
                                            value = Json.serialize np}
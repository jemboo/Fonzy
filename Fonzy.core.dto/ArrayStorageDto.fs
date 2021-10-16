namespace global

type stringMapDto = {vals:string[]}
module StringMapDto = 
    let fromDto (dto:stringMapDto) =
          dto.vals 
            |> Array.chunkBySize 2
            |> Array.map(fun aa -> (aa.[0], aa.[1]))
            |> Map.ofArray
            |> Ok

    let toDto (mp:Map<string,string>) =
        let kd = mp |> Map.toSeq
                    |> Seq.map(fun tup -> seq{fst tup; snd tup})
                    |> Seq.concat
        {stringMapDto.vals = kd |> Seq.toArray}


type intDiffDto = int[] //[|diffIndex; diffValue;|]

type sparseIntArrayDto = { defaultVal:int; len:int; diffs:intDiffDto[] }
module SparseIntArrayDto =

    let fromDto (dto:sparseIntArrayDto) =
        result {
            let diffs = dto.diffs |> Array.map(fun aa -> (aa.[0], aa.[1]))
            let! aLen = ArrayLength.create "" dto.len
            return { 
                     sparseArray.defaultVal = dto.defaultVal;
                     sparseArray.diffs = diffs;
                     sparseArray.len = aLen;
                   }
        }

    let toDto (sia:sparseArray<int>) =
        {
            defaultVal = sia.defaultVal; 
            len = ArrayLength.value(sia.len);
            diffs = sia.diffs |> Array.map(fun tup -> [|fst tup; snd tup|])
        }
        
    let fromJson (cereal:string) =
        result {
            let! dto = Json.deserialize<sparseIntArrayDto> cereal
            return! fromDto dto
        }

    let toJson (sia:sparseArray<int>) = 
        sia |> toDto |> Json.serialize



type sparseIntArraySetDto =  {defVal:int; len:int; sas:sparseIntArrayDto[] }
module SparseIntArraySetDto =
    let fromDto (dto:sparseIntArraySetDto) =
        result {
            let defV = dto.defVal
            let! aLen = ArrayLength.create "" dto.len
            let! sases = dto.sas |> Array.map(SparseIntArrayDto.fromDto)
                        |> Array.toList
                        |> Result.sequence
            return {
                sparseArraySet.defaultVal = defV;
                sparseArraySet.arrayLen = aLen;
                sparseArraySet.aas = sases |> List.toArray
            }
        }

    let toDto (sias:sparseArraySet<int>) =
        {defVal = sias.defaultVal; 
         len =  ArrayLength.value sias.arrayLen;
         sas = sias.aas |> Array.map(SparseIntArrayDto.toDto)}

    let fromJson (cereal:string) =
        result {
            let! dto = Json.deserialize<sparseIntArraySetDto> cereal
            return! fromDto dto
        }

    let toJson (sias:sparseArraySet<int>) =
        sias |> toDto |> Json.serialize



type diffIntArrayDto = { diffs:intDiffDto[] }
module DiffIntArrayDto =

    let fromDto (dto:diffIntArrayDto) =
        result {
            let diffs = dto.diffs |> Array.map(fun aa -> (aa.[0], aa.[1]))
            return { 
                     diffArray.diffs = diffs;
                   }
        }

    let toDto (sia:diffArray<int>) =
        {
            diffs = sia.diffs |> Array.map(fun tup -> [|fst tup; snd tup|])
        }
        
    let fromJson (cereal:string) =
        result {
            let! dto = Json.deserialize<diffIntArrayDto> cereal
            return! fromDto dto
        }

    let toJson (sia:diffArray<int>) = 
        sia |> toDto |> Json.serialize



type diffIntArraySetDto =  { baseArray:int[]; das:diffIntArrayDto[] }
module DiffIntArraySetDto =
    let fromDto (dto:diffIntArraySetDto) =
        result {
            let! sases = dto.das |> Array.map(DiffIntArrayDto.fromDto)
                        |> Array.toList
                        |> Result.sequence
            return {
                diffArraySet.baseArray = dto.baseArray;
                diffArraySet.dfAs = sases |> List.toArray
            }
        }

    let toDto (dfas:diffArraySet<int>) =
        {baseArray =  dfas.baseArray;
         das = dfas.dfAs |> Array.map(DiffIntArrayDto.toDto)}

    let fromJson (cereal:string) =
        result {
            let! dto = Json.deserialize<diffIntArraySetDto> cereal
            return! fromDto dto
        }

    let toJson (sias:diffArraySet<int>) =
        sias |> toDto |> Json.serialize



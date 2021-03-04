namespace global

type PermutationDto = {degree:int; values:int[] }
module PermutationDto =
    let fromDto (dto:PermutationDto) =
        result {
            let! degree = Degree.create "" dto.degree
            return! Permutation.createR degree dto.values
        }
    let toDto (perm:Permutation) =
        {degree = (Degree.value (Permutation.degree perm)); 
         values = Permutation.arrayValues perm}
        
    let fromJson (cereal:string) =
        result {
            let! dto = Json.deserialize<PermutationDto> cereal
            let! degree = Degree.create "" dto.degree
            return! Permutation.createR degree dto.values
        }


type TwoCyclePermDto =  {degree:int; values:int[] }
module TwoCyclePermDto =
    let fromDto (dto:TwoCyclePermDto) =
        result {
            let! degree = Degree.create "" dto.degree
            return! TwoCyclePerm.create degree dto.values
        }
    let toDto (tcp:TwoCyclePerm) =
        {degree = (Degree.value (TwoCyclePerm.degree tcp)); 
         values =  TwoCyclePerm.arrayValues tcp;}

    let fromJson (cereal:string) =
        result {
            let! dto = Json.deserialize<TwoCyclePermDto> cereal
            let! degree = Degree.create "" dto.degree
            return! TwoCyclePerm.create degree dto.values
        }
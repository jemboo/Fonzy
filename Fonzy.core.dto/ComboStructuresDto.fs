namespace global

type permutationDto = {degree:int; values:int[] }
module PermutationDto =
    let fromDto (dto:permutationDto) =
        result {
            let! degree = Degree.create "" dto.degree
            return! Permutation.createR degree dto.values
        }
    let toDto (perm:permutation) =
        {degree = (Degree.value (Permutation.degree perm)); 
         values = Permutation.arrayValues perm}
        
    let fromJson (cereal:string) =
        result {
            let! dto = Json.deserialize<permutationDto> cereal
            let! degree = Degree.create "" dto.degree
            return! Permutation.createR degree dto.values
        }


type twoCyclePermDto =  {degree:int; values:int[] }
module TwoCyclePermDto =
    let fromDto (dto:twoCyclePermDto) =
        result {
            let! degree = Degree.create "" dto.degree
            return! TwoCyclePerm.create degree dto.values
        }
    let toDto (tcp:twoCyclePerm) =
        {degree = (Degree.value (TwoCyclePerm.degree tcp)); 
         values =  TwoCyclePerm.arrayValues tcp;}

    let fromJson (cereal:string) =
        result {
            let! dto = Json.deserialize<twoCyclePermDto> cereal
            let! degree = Degree.create "" dto.degree
            return! TwoCyclePerm.create degree dto.values
        }
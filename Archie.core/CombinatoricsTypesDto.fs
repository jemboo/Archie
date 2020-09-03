namespace Archie.Base

type PermutationDto = private {degree:int; values:int[] }
module PermutationDto =
    let fromDto (dto:PermutationDto) =
        result {
            let! degree = Degree.create "" dto.degree
            return! Permutation.create degree dto.values
        }
    let toDto (perm:Permutation) =
        {degree= (Degree.value perm.degree); values=perm.values}


type TwoCyclePermDto = private {degree:int; values:int[] }
module TwoCyclePermDto =
    let fromDto (dto:TwoCyclePermDto) =
        result {
            let! degree = Degree.create "" dto.degree
            return! TwoCyclePerm.create degree dto.values
        }
    let toDto (tcp:TwoCyclePerm) =
        {degree= (Degree.value tcp.degree); values=tcp.values}


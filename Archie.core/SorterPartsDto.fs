namespace Archie.Base
open System

module SwitchDto =
    let fromDto dex = Switch.switchMap.[dex]
    let toDto (switch:Switch) =
        (switch.hi * (switch.hi + 1)) / 2 + switch.low
     

type SwitchUsesDto = private {switchCount:int; weights:int[]}
module SwitchUsesDto =
    let fromDto (dto:SwitchUsesDto) =
        result {
            let! switchCount = SwitchCount.create "" dto.switchCount
            return! SwitchUses.create switchCount dto.weights
        }

    let toDto (switchUses:SwitchUses) =
        {SwitchUsesDto.switchCount= (SwitchCount.value switchUses.switchCount); 
         weights=switchUses.weights}


type SorterDto = {degree:int; switches:int[]}
module SorterDto =
    let fromDto (dto:SorterDto) =
        result {
            let! degree = Degree.create "" dto.degree
            let switches = dto.switches |> Array.map(fun sw -> SwitchDto.fromDto sw)
            return Sorter.create degree switches
        }
    let toDto (sorter:Sorter) =
        {
            SorterDto.degree = (Degree.value sorter.degree); 
            switches = sorter.switches |> Array.map(fun sw -> SwitchDto.toDto sw)
        }
namespace Archie.Base
open System
open Newtonsoft.Json

module Json = 
    type Marker = interface end

    let serialize obj = JsonConvert.SerializeObject obj

    let deserialize<'a> str :Result<'a, string> =
        try
            JsonConvert.DeserializeObject<'a> str |> Ok
        with
        | ex -> Result.Error ex.Message

    let deserializeOption<'a> str =
        match str with
        | Some s -> (deserialize<'a> s)
        | None -> failwith  "data missing"


type LogFileDto = {cat:string; descr:string; header:string; records:string[]}


type CustomParamsDto = {parseKey:string; prams:Map<string, string>}

type RngGenDto = {rngType:string; seed:int}
module RngGenDto =
    let fromDto (dto:RngGenDto) =
        result {
            let! typ = RngType.create dto.rngType
            let! rs = RandomSeed.create "" dto.seed
            return {RngGen.rngType=typ; seed=rs}
        }
    let toDto (rngGen:RngGen) =
        {rngType=(RngType.toDto rngGen.rngType); 
         seed=RandomSeed.value rngGen.seed}

    let toJson (rngGen:RngGen) =
        rngGen |> toDto |> Json.serialize

    //let fromJson (json:string) :Result<obj, RngGen> =
    //    result {
    //                let! dto = Json.deserialize<RngGenDto> json

    //                let! entObj = domainFromDto dto |> Result.StringErr
    //                return entObj :> obj
    //            }

        
    //let toDto (rngt:RngGen) =
    //    match rngt.rngType with
    //    | Lcg -> sprintf "Lcg %d" (RandomSeed.value rngt.seed)
    //    | Net -> sprintf "Net %d" (RandomSeed.value rngt.seed)

    //let fromDto (str:string) =
    //    let finishParse (str:string) =
    //        match str with
    //        | "Lcg" -> RngType.Lcg |> Ok
    //        | "Net" -> RngType.Net |> Ok
    //        | _ -> Error (sprintf "no match for RngType: %s" str)
    //    let doArgs (pcs:string[]) =
    //        if pcs.Length = 2 then
    //            result {
    //                      let! seed = (ParseUtils.MakeInt32 pcs.[1])
    //                      let! rndSeed = RandomSeed.create "" seed
    //                      let! rng = finishParse pcs.[0]
    //                      return {RngGen.rngType=rng; RngGen.seed=rndSeed;}
    //                   }
    //          else
    //              Error (sprintf "incorrect string: %s" str)
    //    result {
    //        let pcs = str.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    //        return! doArgs pcs
    //    }
    
type MutationTypeDto = {mType:string; rate:float}
module  MutationTypeDto =
    let fromDto (dto:MutationTypeDto) =
        result {
                let! mr = MutationRate.create "" dto.rate
                match dto.mType with
                | "Switch" -> return MutationType.Switch mr
                | "Stage" ->  return MutationType.Stage mr
                | _ ->        let! res =  Error (sprintf "no match for MutationType: %s" dto.mType)
                              return res
            }

    let toDto (mutationType:MutationType) =
        match mutationType with
        | MutationType.Switch mr -> {mType="Switch"; rate=MutationRate.value mr}
        | MutationType.Stage mr -> {mType="Stage"; rate=MutationRate.value mr}



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
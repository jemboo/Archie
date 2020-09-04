namespace Archie.Base


type SorterPhenotyperDto = {parseKey:string; prams:Map<string, string> option}
type SorterTesterDto = {parseKey:string; prams:Map<string, string> option}
type SorterEvaluatorDto = {parseKey:string; prams:Map<string, string> option}



type GaRunDto = private {parseKey:string; prams:Map<string, string>}
module GaRunDto =
    let create (parseKey:string) (kvps:(string*string)[]) =
        result {
            let! map = CollectionUtils.tuplesToMap kvps
            return {parseKey=parseKey; prams=map}
            }

    let serialize (gaRunDto:GaRunDto) =
        Json.serialize gaRunDto

    let deserialize json =
        Json.deserialize<Map<string, string>> json
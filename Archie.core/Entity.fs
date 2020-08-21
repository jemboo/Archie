namespace Archie.Base
open Microsoft.FSharp.Collections
open System

//type EntityId2 = {id:}
   

type Entity<'a> = private { entityId:EntityId; character:'a }
module Entity =
    let id entity = entity.entityId
    let character entity = entity.character
    
    let create (entityId:EntityId) (character:'a) =
            {
                entityId=entityId; 
                character=character;
            }

    let createMany (rando1:IRando) (rando2:IRando) (src:seq<'a>) =
        let wrap rnd1 rnd2 c =
            let id = EntityId.create (Rando.NextGuid2 rnd1 rnd2) |> Result.ExtractOrThrow
            create id c
        src |> Seq.map(fun c -> wrap rando1 rando2 c)

    let GuidSeeds (seed:int) =
        let masterRando = Rando.LcgFromSeed seed
        Seq.initInfinite(fun i -> (i, 
                                    Rando.NextGuid masterRando, 
                                    RandomSeed.create "" masterRando.NextPositiveInt
                                        |> Result.ExtractOrThrow))

    let GuidSeedsA = GuidSeeds 123
    let GuidSeedsB = GuidSeeds 987


type EntitySet<'a> = { map:Map<EntityId, Entity<'a>> }
module EntitySet =
    let create (items:seq<Entity<'a>>) =
            result {
                    let map = items |> Seq.map(fun e-> (Entity.id e), e)
                                    |> Map.ofSeq
                    return { map=map}
                }


type Dependent<'a> = private { entityId:EntityId; character:'a }
module Dependent =
    
    let create (entity:Entity<'b>) (character:'a) =
            {
                entityId=(Entity.id entity); 
                character=character;
            }

    let id entity = entity.entityId

    let createD (entity:Dependent<'b>) (character:'a) =
            {
                entityId=(id entity); 
                character=character;
            }

    let character entity = entity.character

    let map f d = 
        { entityId=(id d); character= f (character d) }

    let format d =
        sprintf "%s %A" (string (EntityId.value (id d))) (character d)

    let formatf f d =
        sprintf "%s %A" (string (EntityId.value (id d))) (f (character d))
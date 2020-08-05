namespace Archie.Base
open Microsoft.FSharp.Collections
open System

type Entity<'a> = private { entityId:EntityId; character:'a }
module Entity =
    
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

    let id entity = entity.entityId

    let character entity = entity.character


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
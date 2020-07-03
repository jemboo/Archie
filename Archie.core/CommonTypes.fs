namespace Archie.Base
open System

type String50 = private String50 of string
type KeyOrder = private KeyOrder of int
type RandomSeed = private RandomSeed of int
type SorterCount = private SorterCount of int
type SorterLength = private SorterLength of int
type SortableCount = private SortableCount of int
type EntityId = private EntityId of Guid
type JsonString = private JsonString of string
type RngType = | Lcg | Net
type IRando =
    abstract member Count: int
    abstract member Seed : RandomSeed 
    abstract member NextUInt : uint32 
    abstract member NextULong : uint64
    abstract member NextFloat : float
    abstract member RngType : RngType


module String50 =
    let value (String50 str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName String50 50 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName String50 50 str

module KeyOrder  =
    let value (KeyOrder v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName KeyOrder 1 1000 v

module RandomSeed =
    let value (RandomSeed seed) = seed
    let create fieldName seed =
        ConstrainedType.createInt fieldName RandomSeed 1 2147483647 seed

module SorterLength  =
    let value (SorterLength v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SorterLength 1 1000 v

module SorterCount  =
    let value (SorterCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SorterCount 1 1000 v

module SortableCount  =
    let value (SortableCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SortableCount 1 1000 v

module EntityId  =
    let value (EntityId v) = v
    let create id = Ok (EntityId id)

module JsonString =
    let value (JsonString str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName JsonString 100000000 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName JsonString 50 str




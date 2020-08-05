﻿namespace Archie.Base
open System

type String50 = private String50 of string
type Degree = private Degree of int
type RandomSeed = private RandomSeed of int
type SorterCount = private SorterCount of int
type SwitchCount = private SwitchCount of int
type StageCount = private StageCount of int
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

module Degree  =
    let value (Degree v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName Degree 1 1000 v

module RandomSeed =
    let value (RandomSeed seed) = seed
    let create fieldName seed =
        ConstrainedType.createInt fieldName RandomSeed 1 2147483647 seed

module SwitchCount  =
    let value (SwitchCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SwitchCount 1 10000 v

module StageCount  =
    let value (StageCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName StageCount 1 1000 v
    let ToSwitchCount (degree:Degree) (stageCount:StageCount) =
        SwitchCount.create "" ((Degree.value degree) * (value stageCount) / 2)

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

module RngType =
    let toDto (rngt: RngType) =
        match rngt with
        | Lcg -> "Lcg"
        | Net -> "Net"
    let create str =
        match str with
        | "Lcg" -> RngType.Lcg |> Ok
        | "Net" -> RngType.Net |> Ok
        | _ -> Error [|(sprintf "no match for RngType: %s" str)|]

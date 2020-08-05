namespace Archie.Base
open System
open Archie.Base
open Archie.Base.SorterParts

module SortersFromData =

    let ParseToStages (degree:Degree) (stagesStr:string) =
        let MakeSwitch (s:string) =
            let pcs = s.Split([|',';|])
                        |> Seq.map(fun i -> i |> int)
                        |> Seq.toArray
            { Switch.low = pcs.[0]; Switch.hi = pcs.[1]}

        stagesStr.Split([|'[';  ']'; '\n'; '\r'; ' ';|], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map(fun l -> l.Split([|'(';  ')';|], StringSplitOptions.RemoveEmptyEntries)
                                |> Seq.filter(fun pc -> pc <> ",")
                                |> Seq.map(fun pcs -> MakeSwitch pcs)
                                |> Seq.toList
                        )
            |> Seq.map(fun sws -> {Stage.switches = sws; degree=degree} )


    let ParseToSwitches (stagesStr:string) (degree:Degree)=
         result {
            let! stages = Result.ErrorOnException (ParseToStages degree) stagesStr
            return stages |> Seq.map(fun s -> s.switches |> List.toSeq)
                          |> Seq.concat
         }


    let ParseToSorter (sorterString:string) (degree:Degree) =
        result {
           let! switchSeq = ParseToSwitches sorterString degree
           let switches = switchSeq |> Seq.toArray
           let! switchCount = SwitchCount.create "" switches.Length 
           return { Sorter.degree = degree; 
                    switchCount = switchCount;
                    switches = switches }
        }


    type RefSorter =
        | Degree8 | Degree8Prefix3 | Degree10
        | Degree11 | Degree12 | Degree13 | Degree14 | Degree15
        | Green16 | End16 | Degree17  | Degree18
        | Degree20 | Degree22 | Degree23 | Degree24
        | Degree25 | Degree26 | Degree28  | Degree32


    module RefSorter =

        let GetStringAndDegree (refSorter:RefSorter) =
            let d (v:int) =
               let qua = (Degree.create "" v) |> Result.toOption
               qua.Value

            match refSorter with
            | Degree8 -> (SorterData.Degree8Str, d 8) | Degree8Prefix3 -> (SorterData.Degree8Prefix3Str, d 8)
            | Degree10 -> (SorterData.Degree10Str, d 10) | Degree11 -> (SorterData.Degree11Str, d 11)
            | Degree12 -> (SorterData.Degree12Str , d 12) | Degree13 -> (SorterData.Degree13Str, d 13)
            | Degree14 -> (SorterData.Degree14Str , d 14) | Degree15 -> (SorterData.Degree15Str, d 15)
            | Green16 -> (SorterData.Degree16_Green, d 16) | End16 -> (SorterData.Degree16_END, d 16)
            | Degree17 -> (SorterData.Degree17Str, d 17) | Degree18 -> (SorterData.Degree18Str, d 18)
            | Degree20 -> (SorterData.Degree20Str, d 20)
            | Degree22 -> (SorterData.Degree22Str, d 22)
            | Degree23 -> (SorterData.Degree23Str, d 23)
            | Degree24 -> (SorterData.Degree24aStr, d 24)
            | Degree25 -> (SorterData.Degree25Str, d 25)
            | Degree26 -> (SorterData.Degree26Str, d 26)
            | Degree28 -> (SorterData.Degree28Str, d 28)
            | Degree32 -> (SorterData.Degree32Str, d 32)


        let CreateRefSorter (refSorter:RefSorter) =
            let (sorterString, degree) = (GetStringAndDegree refSorter)
            ParseToSorter sorterString degree


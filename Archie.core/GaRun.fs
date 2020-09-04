namespace Archie.Base

type RunSorterRandomWalks = {key:string}
module RunSorterRandomWalks =

    let make = None


type GenomeDifference =
| Penotypic
| CriticalRegion
| NonCriticalRegion


type SorterGenome =
| Self of Sorter
| TwoCycles of TwoCyclePerm[]


module SorterGenome =

    let createRandomSorterType (degree:Degree) (sorterLength:SorterLength) 
                               (switchFreq:float option) (rnd:IRando) =
         SorterGenome.Self
            (Sorter.createRandom degree sorterLength switchFreq rnd)

    let createRandomTwoCycleType (degree:Degree) (sorterLength:SorterLength) 
                                 (switchFreq:float) (rnd:IRando) =
        let makeStages count = 
            seq {1..count} |> Seq.map(fun _->
            (TwoCyclePerm.makeRandomTwoCycle degree rnd switchFreq))
            |> Seq.toArray

        let twoCycles = 
            match sorterLength with
            | SorterLength.Switch ct -> makeStages ((SwitchCount.value ct) / (Degree.value degree))
            | SorterLength.Stage ct -> makeStages (StageCount.value ct)

        SorterGenome.TwoCycles twoCycles


type SorterPhenotype = 
| Sorter of Sorter

type SorterPhenotyperDto = {parseKey:string; prams:Map<string, string> option}
module SorterPhenotyper =
    let makeSorterPhenotype (sg:SorterGenome) = 
        match sg with
        | Self s -> s
        | TwoCycles tc -> Sorter.fromTwoCycleArray tc



type SorterTesterDto = {parseKey:string; prams:Map<string, string> option}
module SorterTester =
    let a = None


type StandardSorterTestResults = 
        {
            switchUses:SwitchUses;
            successfulSortCount:SortableCount;
            switchUseCount:SwitchCount;
            stageUseCount:StageCount
        }

type SorterTestResults = 
| Standard of StandardSorterTestResults * SorterTesterDto
module SorterTestResults =
    let a = None


type SorterEvaluatorDto = {parseKey:string; prams:Map<string, string> option}
module SorterEvaluator =
    let a = None


type SorterEvaluation = 
| Standard of float * SorterEvaluatorDto


type SorterGenomeAction = 
    {
        generation:GenerationNumber;
        genome:SorterGenome;
        phenotype:SorterPhenotype;
        testResults:SorterTestResults;
        eval:SorterEvaluation
    }


type SorterOrg =
    | Original of SorterGenomeAction
    | Mutant of SorterGenomeAction * SorterGenomeAction * GenomeDifference
    | ReEvaluation of SorterGenomeAction * SorterGenomeAction

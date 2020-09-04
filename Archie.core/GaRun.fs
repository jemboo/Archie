namespace Archie.Base

type RunSorterRandomWalks = {key:string}
module RunSorterRandomWalks =

    let make = None


type GenomeDifference = | Penotypic
                        | CriticalRegion
                        | NonCriticalRegion


type SorterGenome = | Self of Sorter
                    | TwoCycles of TwoCyclePerm[]

type SorterPhenotype = | Sorter of Sorter

type StandardSorterTestResults = 
    {
        switchUses:SwitchUses;
        successfulSortCount:SortableCount;
        switchUseCount:SwitchCount;
        stageUseCount:StageCount
    }

type SorterTestResults = | Standard of StandardSorterTestResults
type SorterEvaluation = | Standard of float

type SorterGaAction = 
    {
        generation:GenerationNumber;
        genome:SorterGenome;
        phenotype:SorterPhenotype;
        testResults:SorterTestResults;
        eval:SorterEvaluation
    }

type SorterOrg =
    | Original of SorterGaAction
    | Mutant of SorterGaAction * SorterGaAction * GenomeDifference
    | ReEvaluation of SorterGaAction * SorterGaAction


type SorterPool = { generation:GenerationNumber; orgs:SorterOrg[] }
type PoolReport = { reportType:string; report:string }
    
type SorterPhenotyper = SorterGenome -> SorterPhenotype
type SorterTester = SorterPhenotype -> SorterTestResults
type SorterEvaluator = SorterTestResults -> SorterEvaluation
type PoolSelector = SorterPool -> SorterPool
type PoolReporter = SorterPool -> PoolReport
type PoolUpdater = SorterPool -> SorterPool


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

module SorterPhenotyper =
    let makeSorterPhenotype (sg:SorterGenome) = 
        match sg with
        | Self s -> s
        | TwoCycles tc -> Sorter.fromTwoCycleArray tc


module SorterTester =
    let CompleteSort = None



module SorterTestResults =
    let a = None


module SorterEvaluator =
    let a = None




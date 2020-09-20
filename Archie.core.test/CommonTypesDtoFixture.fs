namespace Archie.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Archie.Base
open Newtonsoft.Json

[<TestClass>]
type CommonTypesDtoFixture () =

    [<TestMethod>]
    member this.TestParseRandGenMode() =
        let lfDto = {LogFileDto.cat="cat"; descr="descr"; header="header"; records=[|"a"; "b"|]}
        let dto = JsonConvert.SerializeObject lfDto
        let lfDtoRet = JsonConvert.DeserializeObject<LogFileDto> dto
        System.Console.WriteLine(dto)
        Assert.AreEqual(lfDto, lfDtoRet)

        //{"cat":"cat","descr":"descr","header":"header","records":["a","b"]}
    [<TestMethod>]
    member this.FileIo() =
        let fp = "c:\log\JsonTest.txt"
        //let lfDto = {LogFileDto.cat="cat"; descr="descr"; header="header"; records=[|"a"; "b"|]}
        //let dtoOut = JsonConvert.SerializeObject lfDto
        //System.IO.File.WriteAllText(fp, dtoOut)
        let dto = System.IO.File.ReadAllText(fp)
        let lfDtoRet = JsonConvert.DeserializeObject<LogFileDto> dto
        Assert.AreEqual(1, 1)

    [<TestMethod>]
    member this.MapSer() =
        let kvps = [|("a","a"); ("b", "b"); ("c", "c"); ("d", "d") |]
                    |> Map.ofArray
        let json = JsonConvert.SerializeObject(kvps)
        let kvpsB = JsonConvert.DeserializeObject<Map<string,string>>(json)
        Assert.AreEqual(kvps, kvpsB)


    [<TestMethod>]
    member this.MutationTypeSer() =
        let mt = SorterMutationType.Stage (MutationRate.fromFloat 0.1)
        let json = SorterMutationTypeDto.toJson mt
        let mtBack = SorterMutationTypeDto.fromJson json |> Result.ExtractOrThrow
        Assert.AreEqual(mt,mtBack)
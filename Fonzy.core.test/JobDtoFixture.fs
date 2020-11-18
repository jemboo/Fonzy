namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type JobDtoFixture () =

    [<TestMethod>]
    member this.JobFile () =
        let worldId = Guid.NewGuid()
        let world = World.create worldId None Cause.noOp Enviro.Empty
        let worldDto = WorldDto.toDto world
        let jobFile = JobFile.ReadWorld worldDto
        let jfDto = JobFileDto.toDto jobFile
        let jfJson = Json.serialize jfDto
        let jfDtoBack = Json.deserialize<JobFileDto> jfJson |> Result.ExtractOrThrow
        let jobFileBack = JobFileDto.fromDto jfDtoBack |> Result.ExtractOrThrow
        Assert.AreEqual(jobFile, jobFileBack);


    [<TestMethod>]
    member this.JobDto () =
        let worldId = Guid.NewGuid()
        let world = World.create worldId None Cause.noOp Enviro.Empty
        let job = Job.GetWorld world
        let jobDto = JobDto.toDto job
        let jobJson = Json.serialize jobDto
        let jobDtoBack = Json.deserialize<JobDto> jobJson |> Result.ExtractOrThrow
        let jobBack = JobDto.fromDto jobDtoBack |> Result.ExtractOrThrow
        Assert.AreEqual(Job.getId job, Job.getId jobBack);
        Assert.AreEqual(Job.getParentId job, Job.getParentId jobBack);
        Assert.AreEqual(Job.getCause job, Job.getCause jobBack);
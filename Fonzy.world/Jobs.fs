namespace global
open System


type Job = 
     | GetWorld of World
     | MakeWorld of WorldAction

module Job =

    let getId (job:Job) = 
        match job with
        | GetWorld w -> w.id
        | MakeWorld wa -> wa.childId

    let getParentId (job:Job) = 
        match job with
        | GetWorld w -> w.parentId
        | MakeWorld wa -> wa.parentWorld.id

    let getCause (job:Job) = 
        match job with
        | GetWorld w -> w.cause
        | MakeWorld wa -> wa.cause

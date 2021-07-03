namespace global
open System

type Sorter = {degree:Degree; 
               switches:array<Switch>; 
               switchCount:SwitchCount}

module Sorter =

    let makeId (s:Sorter) = 
        let gu = [s :> obj] |> GuidUtils.guidFromObjList
        SorterId.fromGuid gu

    let create (degree:Degree) (switches:seq<Switch>) =
        let switchArray = switches |> Seq.toArray
        let switchCount = SwitchCount.fromInt switchArray.Length
        {
            Sorter.degree=degree;
            switchCount=switchCount;
            switches = switchArray
        }
   
    let appendSwitches (switches:seq<Switch>) (sorter:Sorter) =
        let newSwitches = (switches |> Seq.toArray) |> Array.append sorter.switches
        let newSwitchCount = SwitchCount.create "" newSwitches.Length |> Result.toOption
        {
            Sorter.degree = sorter.degree;
            switchCount=newSwitchCount.Value;
            switches = (switches |> Seq.toArray) |> Array.append sorter.switches
        }

    let trimLength (sorter:Sorter) (newLength:SwitchCount) =
        if (SwitchCount.value sorter.switchCount) < (SwitchCount.value newLength) then
            "New length is longer than sorter" |> Error
        else
        let newSwitches = sorter.switches |> Array.take (SwitchCount.value newLength)
        {
            Sorter.degree = sorter.degree;
            switchCount = newLength;
            switches = newSwitches
        } |> Ok

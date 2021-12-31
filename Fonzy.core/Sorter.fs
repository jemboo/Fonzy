namespace global
open System
open FSharpx.Collections

type sorter = 
    { 
        degree:Degree; 
        switches:array<Switch>; 
        switchCount:SwitchCount
    }

module Sorter =

    let makeId (s:sorter) = 
        let gu = [s :> obj] |> GuidUtils.guidFromObjs
        SorterId.fromGuid gu


    let fromSwitches (degree:Degree) 
                     (switches:seq<Switch>) =
        let switchArray = switches |> Seq.toArray
        let switchCount = SwitchCount.fromInt switchArray.Length
        {
            sorter.degree=degree;
            switchCount=switchCount;
            switches = switchArray
        }


    let fromStages (degree:Degree) 
                   (stages:seq<Stage>) =
        let switchArray = stages |> Seq.map(fun st->st.switches)
                                    |> Seq.concat
                                    |> Seq.toArray
        let switchCount = SwitchCount.fromInt switchArray.Length
        {
            sorter.degree=degree;
            switchCount=switchCount;
            switches = switchArray
        }
   

    let appendSwitches (switches:seq<Switch>) 
                       (sorter:sorter) =
        let newSwitches = (switches |> Seq.toArray) |> Array.append sorter.switches
        let newSwitchCount = SwitchCount.create "" newSwitches.Length |> Result.toOption
        {
            sorter.degree = sorter.degree;
            switchCount=newSwitchCount.Value;
            switches = newSwitches
        }


    let trimLength 
            (trimFromEnd: bool)
            (newLength:SwitchCount) 
            (sorter:sorter) =
        if (SwitchCount.value sorter.switchCount) < (SwitchCount.value newLength) then
            "New length is longer than sorter" |> Error
        else
        let newSwitches =  match trimFromEnd with 
                           | true -> sorter.switches |> Array.take (SwitchCount.value newLength)
                           | _ -> sorter.switches |> Array.skip((sorter.switches.Length) - (SwitchCount.value newLength))
        {
            sorter.degree = sorter.degree;
            switchCount = newLength;
            switches = newSwitches
        } |> Ok


    let getSwitchPrefix (stageCount:StageCount) 
                        (sorter:sorter) =
        sorter.switches |> Stage.fromSwitches sorter.degree
                        |> Seq.take(StageCount.value stageCount)
                        |> Seq.map(fun t -> t.switches)
                        |> Seq.concat
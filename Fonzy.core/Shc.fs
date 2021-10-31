namespace global
open System

type sHC<'T,'A> = 
    {
       id:ShcId;
       current: 'T;
       archive: 'A list;
       mutator: 'T -> Result<'T, string>
       evaluator: 'T -> Result<'T, string>
       annealer: 'T -> 'T -> Result<'T, string>
       updater: 'A list -> 'T -> Result<'A list, string>
       terminator: 'T -> bool
    }

    
type sHCset<'S,'T,'A> =  
    {
        specs:Map<ShcId,'S>;
        members:Map<ShcId, Result<sHC<'T,'A>, string>>;
    }



module SHC =

    let newGen  (mutator:'A -> Result<'A, string>)
                (evaluator:'A -> Result<'A, string>) 
                (curGen:'A) =
        result {
            let! aMut = curGen |> mutator
            return! aMut |> evaluator
        }

    let update (shc:sHC<'T,'A>) =
        if shc.archive.Length = 0 then
            result {
                let! updated = shc.current |> shc.evaluator
                let! aNext = shc.annealer shc.current updated
                let! aLst = aNext |> shc.updater shc.archive
                return
                    {
                        sHC.id = shc.id;
                        sHC.current = aNext;
                        sHC.archive = aLst;
                        sHC.mutator = shc.mutator;
                        sHC.updater = shc.updater;
                        sHC.evaluator = shc.evaluator;
                        sHC.annealer = shc.annealer;
                        sHC.terminator = shc.terminator;
                    }
            }
        else
            result {
                let! updated = shc.current |> newGen shc.mutator shc.evaluator
                let! aNext = shc.annealer shc.current updated
                let! aLst = aNext |> shc.updater shc.archive
                return
                    {
                        sHC.id = shc.id;
                        sHC.current = aNext;
                        sHC.archive = aLst;
                        sHC.mutator = shc.mutator;
                        sHC.updater = shc.updater;
                        sHC.evaluator = shc.evaluator;
                        sHC.annealer = shc.annealer;
                        sHC.terminator = shc.terminator;
                    }
            }

    //let run (shc:sHC<'T,'A>) =
    //    let goOn (s) = 
    //        not (s.terminator s.current)
    //    result {
    //        let mutable shcCur = shc
    //        while (goOn shcCur) do
    //            let! shcNew = shcCur |> update
    //            shcCur <- shcNew
    //        return shcCur
    //    }

    let run (shc:sHC<'T,'A>) =
        let goOn (s) = 
            not (s.terminator s.current)
        let mutable shcCur = shc
        while (goOn shcCur) do
            let shcNew = shcCur |> update |> Result.ExtractOrThrow
            shcCur <- shcNew
        shcCur |> Ok



//    let runBatch (shcs:sHC<'T,'A>[]) =
//        let ree = shcs |> Array.Parallel.map(run)
//        ree



module sHCset = 
    let make<'S,'T,'A> (idGen: 'S->ShcId)
                       (maker: 'S->Result<sHC<'T,'A>, string>) 
                       (specs: seq<'S>) =
        let specA = specs |> Seq.toArray
        let specMap = specA |> Seq.map(fun s -> (idGen s, s))
                            |> Map.ofSeq
        let memberMap = specA |> Seq.map(fun s -> (idGen s, maker s))
                              |> Map.ofSeq

        {sHCset.specs= specMap; sHCset.members = memberMap}


    let runBatch (useP:UseParallel) 
                 (shcs:sHCset<'S,'T,'A>) = 
        let _runn (id:ShcId) (shcr:Result<sHC<'T,'A>, string>) =
            match shcr with
            | Ok shc -> Console.WriteLine(sprintf "%A" id)
                        (id, SHC.run shc)
            | Error m -> (id, sprintf "error creating spec: %s" m |> Error)
        

        let mms = 
            match UseParallel.value(useP) with
            | true  -> shcs.members 
                        |> Map.toArray
                        |> Array.Parallel.map(fun tup -> _runn (fst tup) (snd tup))
                        |> Map.ofSeq
            | false -> shcs.members 
                        |> Map.toArray
                        |> Array.map(fun tup -> _runn (fst tup) (snd tup))
                        |> Map.ofSeq

        {shcs with members = mms}



//    let runBatch (shcs:sHCset<'S,'T,'A>) = 
//        let _runn (id:ShcId) (shcr:Result<sHC<'T,'A>, string>) =
//            match shcr with
//            | Ok shc -> (id, SHC.run shc)
//            | Error m -> (id, sprintf "error creating spec: %s" m |> Error)
            
//        let mms = shcs.members |> Map.toArray
//                               |> Array.Parallel.map(fun tup -> _runn (fst tup) (snd tup))
//                               |> Map.ofSeq
//        {shcs with members = mms}

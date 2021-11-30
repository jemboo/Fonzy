namespace global
open System

type sHC2<'T> = 
    {
       id:ShcId;
       current: 'T;
       mutator: 'T -> Result<'T, string>
       evaluator: 'T -> Result<'T, string>
       annealer: 'T -> 'T -> Result<'T, string>
       terminator: 'T -> bool
    }

type sHCstate =
     | PostMutate
     | PostEvaluate
     | PostAnnealer
    
type sHCset2<'S,'T> =  
    {
        specMap:Map<ShcId,'S>;
        memberMap:Map<ShcId, Result<sHC2<'T>, string>>;
    }


module SHC2 =

    let update (logger: sHCstate->'T->unit)  (shc:sHC2<'T>) =
            result {
                let! tMut = shc.current |> shc.mutator
                logger sHCstate.PostMutate tMut
                let! tEval = tMut |> shc.evaluator
                logger sHCstate.PostEvaluate tEval
                let! aNext = shc.annealer shc.current tEval
                logger sHCstate.PostAnnealer aNext
                return
                    {
                        sHC2.id = shc.id;
                        current = aNext;
                        mutator = shc.mutator;
                        evaluator = shc.evaluator;
                        annealer = shc.annealer;
                        terminator = shc.terminator;
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

    let evalT (shc:sHC2<'T>) =
        result {
            let! tEval = shc.current |> shc.evaluator
            return
                {
                    sHC2.id = shc.id;
                    current = tEval;
                    mutator = shc.mutator;
                    evaluator = shc.evaluator;
                    annealer = shc.annealer;
                    terminator = shc.terminator;
                }
         }
    

    let run (logger: sHCstate->'T->unit)  (shc:sHC2<'T>) =
        let goOn (s) = 
            not (s.terminator s.current)
        let firstEval = shc |> evalT |> Result.ExtractOrThrow
        let mutable shcCur = firstEval
        while (goOn shcCur) do
            let shcNew = shcCur |> update logger |> Result.ExtractOrThrow
            shcCur <- shcNew
        shcCur |> Ok



module SHCset2 = 

    let make<'S,'T> (idGen: 'S->ShcId)
                    (maker: 'S->Result<sHC2<'T>, string>) 
                    (specs: seq<'S>) =
        let specA = specs |> Seq.toArray
        let specMap = specA |> Seq.map(fun s -> (idGen s, s))
                            |> Map.ofSeq
        let memberMap = specA |> Seq.map(fun s -> (idGen s, maker s))
                              |> Map.ofSeq

        { sHCset2.specMap = specMap; memberMap = memberMap }


    let runBatch (useP:UseParallel) 
                 (logger: sHCstate->'T->unit)
                 (shcs:sHCset2<'S,'T>) = 
        let _runn (id:ShcId) (shcr:Result<sHC2<'T>, string>) =
            match shcr with
            | Ok shc -> Console.WriteLine(sprintf "%A" id)
                        (id, SHC2.run logger shc)
            | Error m -> (id, sprintf "error creating spec: %s" m |> Error)
        

        let mms = 
            match UseParallel.value(useP) with
            | true  -> shcs.memberMap 
                        |> Map.toArray
                        |> Array.Parallel.map(fun tup -> _runn (fst tup) (snd tup))
                        |> Map.ofSeq
            | false -> shcs.memberMap 
                        |> Map.toArray
                        |> Array.map(fun tup -> _runn (fst tup) (snd tup))
                        |> Map.ofSeq

        {shcs with memberMap = mms}

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

    let update (monitor:obj->unit)  //(monitor:obj->unit) 
               (shc:sHC2<'T>) 
               (spec:'S) =
            result {
                let! tMut = shc.current |> shc.mutator
                monitor ((sHCstate.PostMutate, tMut, shc, spec) :> obj)
                let! tEval = tMut |> shc.evaluator
                monitor ((sHCstate.PostEvaluate, tEval, shc, spec) :> obj)
                let! aNext = shc.annealer shc.current tEval
                monitor ((sHCstate.PostAnnealer, aNext, shc, spec) :> obj)
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
    

    let run (monitor:obj->unit)  
            (shc:sHC2<'T>) 
            (spec:'S) =
        let goOn (s) = 
            not (s.terminator s.current)
        let firstEval = shc |> evalT |> Result.ExtractOrThrow
        let mutable shcCur = firstEval
        while (goOn shcCur) do
            let shcNew = update monitor shcCur spec |> Result.ExtractOrThrow
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
                 (context:obj*(obj->Result<obj->unit, string>))
                 (shcs:sHCset2<'S,'T>) = 

        let _runn (id:ShcId) (shcr:Result<sHC2<'T>, string>) =
                result 
                    {
                        let batchSpec, mm = context
                        let! shc = shcr
                        let shcsArg = (shcs.specMap.[id], shcs.memberMap.[id])
                        let! monitor = mm shcsArg
                        Console.WriteLine(sprintf "%A" id)
                        let shcNew = SHC2.run monitor shc (shcs.specMap.Item id)
                        return (id, shcNew)
                    } |> Result.ExtractOrThrow

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

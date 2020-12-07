namespace global
open System


type GridLocation = {id:Guid; x:int; y:int}

type PoolOfGridLocations = {id:Guid; locPool:Map<Guid, GridLocation>}

module PoolOfGridLocations =

    let create (poolId:Guid) (ids:seq<Guid>) (locs: GridLocation[]) =
        let locPool = locs |> Seq.map(fun loc-> (loc.id, loc))
                          |> Map.ofSeq
        {
            PoolOfGridLocations.id=poolId;
            PoolOfGridLocations.locPool=locPool;
        }

    let makePoolOfGridLocations (poolId:Guid) (ids:seq<Guid>) 
                                (locator:Guid->GridLocation) = 
        let locPool = ids |> Seq.map(fun gu-> (gu, locator gu))
                          |> Map.ofSeq
        {
            PoolOfGridLocations.id=poolId;
            PoolOfGridLocations.locPool=locPool;
        }


    let getGridLocations (pogl:PoolOfGridLocations) =
        pogl.locPool |> Map.toSeq
                     |> Seq.map(snd)


    let makeOrigin (poolId:Guid) (ids:seq<Guid>) =
        let locator gu =
            {GridLocation.id=gu; x=0; y=0}
        makePoolOfGridLocations poolId ids locator


    let makeRandomTorusDist (poolId:Guid) (ids:seq<Guid>)
                            (xRadius:int) (yRadius:int) 
                            (rando:IRando) = 
        let nextInt max = (int rando.NextUInt) % max
        let locator gu = { GridLocation.id=gu; x=nextInt xRadius; y=nextInt yRadius; }
        makePoolOfGridLocations poolId ids locator


    let diffuse (newPoolId:Guid) (poolOfGridLocations:PoolOfGridLocations)
                (xRadius:int option) (yRadius:int option)
                (diffuser:IRando->int*int) (rando:IRando) =

        let nextTuple (curr:int) (delta:int) (max: int option) = 
            match max with
            | Some amt -> (curr + delta) % amt
            | None -> (curr + delta)

        let nextLoc currX currY = 
            let delta = diffuser rando
            ((nextTuple currX (fst delta) xRadius), 
             (nextTuple currY (snd delta) yRadius))

        let nextGridLocation (gridLocation:GridLocation) = 
            let newLoc = nextLoc gridLocation.x gridLocation.y
            {
                GridLocation.id = gridLocation.id;
                GridLocation.x = (fst newLoc);
                GridLocation.y = (snd newLoc)
            }

        let newMap =
            poolOfGridLocations.locPool 
                        |> Map.toSeq
                        |> Seq.map(fun kvp-> nextGridLocation (snd kvp))
                        |> Seq.map(fun gridLoc-> (gridLoc.id, gridLoc))
                        |> Map.ofSeq
        {
            PoolOfGridLocations.id = newPoolId;
            PoolOfGridLocations.locPool = newMap
        }

    let gaussianDiffuse (xRadius:int option) (yRadius:int option)
                        (stdDev:float) (rando:IRando)
                        (poolOfGridLocations:PoolOfGridLocations)
                        (newPoolId:Guid) =
    
        let gaussianDiffuser (randy:IRando) = 
            Rando.normalDistIntPair 0.0 stdDev randy

        diffuse newPoolId poolOfGridLocations xRadius yRadius
                          gaussianDiffuser rando

            
    





namespace global
open Microsoft.FSharp.Core


type LatticeLoc2d = {x:int; y:int}

module LatticeLoc2d = 
    let add (xMax:int option) (yMax:int option) 
            (ll2d:LatticeLoc2d) 
            (dx:int) (dy:int) = 
        match xMax, yMax with
        | Some xm, Some ym -> {LatticeLoc2d.x = (ll2d.x + dx) % xm; 
                               y = (ll2d.y + dy) % ym}
        | Some xm, None -> {LatticeLoc2d.x = (ll2d.x + dx) % xm; 
                               y = ll2d.y + dy}
        | None, Some ym -> {LatticeLoc2d.x = ll2d.x + dx; 
                               y = (ll2d.y + dy) % ym}
        | None, None -> {LatticeLoc2d.x = ll2d.x + dx; 
                               y = ll2d.y + dy}

    let fromFltTuple (tup:float*float) =
        {LatticeLoc2d.x = int (fst tup); LatticeLoc2d.y = int (snd tup)}

    let perturb (xMax:int option) (yMax:int option)
                (perturber:IRando->int*int)
                (ll2d:LatticeLoc2d)
                (rando:IRando) = 
        let delta = perturber rando
        add xMax yMax ll2d (fst delta) (snd delta)

    let makeUniformRandom (xMin:int) (xMax:int) (yMin:int) (yMax:int)
                          (rando:IRando) = 
        let xSpan = xMax - xMin
        let ySpan = yMax - yMin
        {LatticeLoc2d.x = xMin + int (rando.NextUInt % (uint32 xSpan)); 
                      y = yMin + int (rando.NextUInt % (uint32 ySpan))}

    let gaussianDiffuse (xRadius:int option) 
                        (yRadius:int option)
                        (stdDevX:float)
                        (stdDevY:float) 
                        (rando:IRando)
                        (latticeLocations:seq<LatticeLoc2d>) =
        let gaussianDiffuser (randy:IRando) = 
            Rando.normalDistInt2d 0.0 stdDevX 0.0 stdDevY randy

        latticeLocations |> Seq.map(fun ll-> perturb xRadius yRadius gaussianDiffuser ll rando)

    let makeLattice2d (xMin:int) (yMin:int) (xMax:int) (yMax:int) =
        let xVals = seq {xMin..xMax} 
        let yVals = seq {yMin..yMax} 
        seq {for x in xVals do for y in yVals -> {LatticeLoc2d.x=x; y=y}}

type LatticeLoc3d = {x:int; y:int; z:int}

module LatticeLoc3d = 
    let add (xMax:int option) (yMax:int option) (zMax:int option) 
            (ll3d:LatticeLoc3d) 
            (dx:int) (dy:int) (dz:int) = 
        match xMax, yMax, zMax with
        | Some xm, Some ym, Some zm -> {LatticeLoc3d.x = (ll3d.x + dx) % xm; 
                                        y = (ll3d.y + dy) % ym; 
                                        z = (ll3d.z + dz) % zm}
        | Some xm, None, Some zm -> {LatticeLoc3d.x = (ll3d.x + dx) % xm; 
                                        y = ll3d.y + dy; 
                                        z = (ll3d.z + dz) % zm}
        | None, Some ym, Some zm -> {LatticeLoc3d.x = ll3d.x + dx; 
                                     y = (ll3d.y + dy) % ym; 
                                     z = (ll3d.z + dz) % zm}
        | None, None, Some zm -> {LatticeLoc3d.x = ll3d.x + dx; 
                                  y = ll3d.y + dy; 
                                  z = (ll3d.z + dz) % zm}
        | Some xm, Some ym, None -> {LatticeLoc3d.x = (ll3d.x + dx) % xm; 
                                  y = (ll3d.y + dy) % ym; 
                                  z = ll3d.z + dz}
        | Some xm, None, None -> {LatticeLoc3d.x = (ll3d.x + dx) % xm; 
                                  y = ll3d.y + dy; 
                                  z = ll3d.z + dz}
        | None, Some ym, None -> {LatticeLoc3d.x = ll3d.x + dx; 
                                  y = (ll3d.y + dy) % ym; 
                                  z = ll3d.z + dz}
        | None, None, None -> {LatticeLoc3d.x = ll3d.x + dx; 
                               y = ll3d.y + dy; 
                               z = ll3d.z + dz}


    let perturb (xMax:int option) (yMax:int option) (zMax:int option)
                (perturber:IRando->int*int*int)
                (ll3d:LatticeLoc3d) 
                (rando:IRando) = 
        let x,y,z = perturber rando
        add xMax yMax zMax ll3d x y z

    let makeUniformRandom (xMax:int) (yMax:int) (zMax:int)
                   (rando:IRando) = 
        {LatticeLoc3d.x = int (rando.NextUInt % (uint32 xMax)); 
                      y = int (rando.NextUInt % (uint32 yMax));
                      z = int (rando.NextUInt % (uint32 zMax))}

    let makeLattice3d (xMin:int) (yMin:int) 
                      (zMin:int) (xMax:int) 
                      (yMax:int) (zMax:int) =
            let xVals = seq {xMin..xMax} 
            let yVals = seq {yMin..yMax}
            let zVals = seq {zMin..zMax} 
            seq {for x in xVals do 
                        for y in yVals do 
                            for z in zVals  -> 
                                {LatticeLoc3d.x=x; y=y; z=z} }


type IntDist = {intDistType:IntDistType; vals:int[]; }

module IntDist = 
    let makeRandom (idt:IntDistType) (r:IRando) (count:int) =
        let ma dt =
            match dt with
            | IntDistType.Uniform uidp -> Array.init count (fun _ -> r.NextPositiveInt)
            | IntDistType.Normal nidp -> Rando.normalDistRandomSeq nidp.mean nidp.stdDev r
                                            |> Seq.map(fun v -> (int v))
                                            |> Seq.take count
                                            |> Seq.toArray

        {IntDist.intDistType=idt; vals = ma idt}

type Lattice2dDist = {lattice2dDistType:Lattice2dDistType; vals:LatticeLoc2d[]; }

module Lattice2dDist =
    let makeRandom (l2dt:Lattice2dDistType) (r:IRando) (count:int) =
        let ma dt =
            match dt with
            | Lattice2dDistType.Uniform uldp -> Array.init count (fun _ -> 
                            LatticeLoc2d.makeUniformRandom uldp.minX uldp.maxX uldp.minY uldp.maxY r)
            | Lattice2dDistType.Normal nldp -> Array.init count (fun _ -> 
                            Rando.normalDistRandomPair 
                                     nldp.meanX nldp.stdDevX nldp.meanY nldp.stdDevY r
                            |> LatticeLoc2d.fromFltTuple)

        {Lattice2dDist.lattice2dDistType=l2dt; vals = ma l2dt}
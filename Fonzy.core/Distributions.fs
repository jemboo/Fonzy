﻿namespace global
open Microsoft.FSharp.Core


type Int2d = {x:int; y:int}
module Int2d = 
    let add (xMax:int option) (yMax:int option) 
            (ll2d:Int2d) 
            (dx:int) (dy:int) = 
        match xMax, yMax with
        | Some xm, Some ym -> {Int2d.x = (ll2d.x + dx) % xm; 
                               y = (ll2d.y + dy) % ym}
        | Some xm, None -> {Int2d.x = (ll2d.x + dx) % xm; 
                               y = ll2d.y + dy}
        | None, Some ym -> {Int2d.x = ll2d.x + dx; 
                               y = (ll2d.y + dy) % ym}
        | None, None -> {Int2d.x = ll2d.x + dx; 
                               y = ll2d.y + dy}

    let fromFltTuple (tup:float*float) =
        {Int2d.x = int (fst tup); Int2d.y = int (snd tup)}

    let perturb (xMax:int option) (yMax:int option)
                (perturber:IRando->int*int)
                (ll2d:Int2d)
                (rando:IRando) = 
        let delta = perturber rando
        add xMax yMax ll2d (fst delta) (snd delta)

    let makeUniformRandom (xMin:int) (xMax:int) (yMin:int) (yMax:int)
                          (rando:IRando) = 
        let xSpan = xMax - xMin
        let ySpan = yMax - yMin
        {Int2d.x = xMin + int (rando.NextUInt % (uint32 xSpan)); 
                      y = yMin + int (rando.NextUInt % (uint32 ySpan))}

    let gaussianDiffuse (xRadius:int option) 
                        (yRadius:int option)
                        (stdDevX:float)
                        (stdDevY:float) 
                        (rando:IRando)
                        (latticeLocations:seq<Int2d>) =
        let gaussianDiffuser (randy:IRando) = 
            Rando.normalDistInt2d 0.0 stdDevX 0.0 stdDevY randy

        latticeLocations |> Seq.map(fun ll-> perturb xRadius yRadius gaussianDiffuser ll rando)

    let makeInt2d (xMin:int) (yMin:int) (xMax:int) (yMax:int) =
        let xVals = seq {xMin..xMax} 
        let yVals = seq {yMin..yMax} 
        seq {for x in xVals do for y in yVals -> {Int2d.x=x; y=y}}

type Int3d = {x:int; y:int; z:int}
module Int3d = 
    let add (xMax:int option) (yMax:int option) (zMax:int option) 
            (ll3d:Int3d) 
            (dx:int) (dy:int) (dz:int) = 
        match xMax, yMax, zMax with
        | Some xm, Some ym, Some zm -> {Int3d.x = (ll3d.x + dx) % xm; 
                                        y = (ll3d.y + dy) % ym; 
                                        z = (ll3d.z + dz) % zm}
        | Some xm, None, Some zm -> {Int3d.x = (ll3d.x + dx) % xm; 
                                        y = ll3d.y + dy; 
                                        z = (ll3d.z + dz) % zm}
        | None, Some ym, Some zm -> {Int3d.x = ll3d.x + dx; 
                                     y = (ll3d.y + dy) % ym; 
                                     z = (ll3d.z + dz) % zm}
        | None, None, Some zm -> {Int3d.x = ll3d.x + dx; 
                                  y = ll3d.y + dy; 
                                  z = (ll3d.z + dz) % zm}
        | Some xm, Some ym, None -> {Int3d.x = (ll3d.x + dx) % xm; 
                                  y = (ll3d.y + dy) % ym; 
                                  z = ll3d.z + dz}
        | Some xm, None, None -> {Int3d.x = (ll3d.x + dx) % xm; 
                                  y = ll3d.y + dy; 
                                  z = ll3d.z + dz}
        | None, Some ym, None -> {Int3d.x = ll3d.x + dx; 
                                  y = (ll3d.y + dy) % ym; 
                                  z = ll3d.z + dz}
        | None, None, None -> {Int3d.x = ll3d.x + dx; 
                               y = ll3d.y + dy; 
                               z = ll3d.z + dz}


    let perturb (xMax:int option) (yMax:int option) (zMax:int option)
                (perturber:IRando->int*int*int)
                (ll3d:Int3d) 
                (rando:IRando) = 
        let x,y,z = perturber rando
        add xMax yMax zMax ll3d x y z

    let makeUniformRandom (xMax:int) (yMax:int) (zMax:int)
                   (rando:IRando) = 
        {Int3d.x = int (rando.NextUInt % (uint32 xMax)); 
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
                                {Int3d.x=x; y=y; z=z} }

type UniformIntegerDistParams = {min:int; max:int}
module UniformIntegerDistParams =

    let zeroCentered (side:int) =
        {
            UniformIntegerDistParams.min = -side + 1;
            UniformIntegerDistParams.max = side;
        }

    let positiveSeg (side:int) =
        {
            UniformIntegerDistParams.min = 0;
            UniformIntegerDistParams.max = side;
        }

type NormalIntegerDistParams = {mean:float; stdDev:float}
module NormalIntegerDistParams = 
    let zeroCentered (stdev:float) =
        {
            NormalIntegerDistParams.mean = 0.0;
            NormalIntegerDistParams.stdDev = stdev;
        }


type IntDistType =
        | Uniform of UniformIntegerDistParams
        | Normal of NormalIntegerDistParams


type IntDist = {intDistType:IntDistType; vals:int[]; }
module IntDist = 
    let makeRandom (idt:IntDistType) (r:IRando) (count:int) =
        let ma dt =
            match dt with
            | IntDistType.Uniform uidp -> Array.init count (fun _ -> 
                    uidp.min + r.NextPositiveInt % (uidp.max - uidp.min))
            | IntDistType.Normal nidp -> Rando.normalDistRandomSeq nidp.mean nidp.stdDev r
                                            |> Seq.map(fun v -> (int v))
                                            |> Seq.take count
                                            |> Seq.toArray

        {IntDist.intDistType=idt; vals = ma idt}


type UniformInt2dDistParams = {minX:int; maxX:int; minY:int; maxY:int}
module UniformInt2dDistParams = 
    let square (side:int) =
        {
            UniformInt2dDistParams.minX = 0;
            UniformInt2dDistParams.maxX = side;
            UniformInt2dDistParams.minY = 0;
            UniformInt2dDistParams.maxY = side;
        }

type NormalInt2dDistParams = {meanX:float; meanY:float; stdDevX:float; stdDevY:float}
module NormalInt2dDistParams = 
    let round (dev:float) =
        {
            NormalInt2dDistParams.meanX = 0.0;
            NormalInt2dDistParams.meanY = 0.0;
            NormalInt2dDistParams.stdDevX = dev;
            NormalInt2dDistParams.stdDevY = dev;
        }

type Int2dDistType =
    | Uniform of UniformInt2dDistParams
    | Normal of NormalInt2dDistParams


type Int2dDist = {lattice2dDistType:Int2dDistType; vals:Int2d[]; }
module Int2dDist =
    let makeRandom (l2dt:Int2dDistType) (r:IRando) (count:int) =
        let ma dt =
            match dt with
            | Int2dDistType.Uniform uldp -> Array.init count (fun _ -> 
                            Int2d.makeUniformRandom uldp.minX uldp.maxX uldp.minY uldp.maxY r)
            | Int2dDistType.Normal nldp -> Array.init count (fun _ -> 
                            Rando.normalDistRandomPair 
                                     nldp.meanX nldp.stdDevX nldp.meanY nldp.stdDevY r
                            |> Int2d.fromFltTuple)

        {Int2dDist.lattice2dDistType=l2dt; vals = ma l2dt}
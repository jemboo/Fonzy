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

    let perturb (xMax:int option) (yMax:int option)
                (perturber:IRando->int*int)
                (ll2d:LatticeLoc2d) 
                (rando:IRando) = 
        let delta = perturber rando
        add xMax yMax ll2d (fst delta) (snd delta)

    let makeUniformRandom (xMax:int) (yMax:int)
                   (rando:IRando) = 
        {LatticeLoc2d.x = int (rando.NextUInt % (uint32 xMax)); 
                      y = int (rando.NextUInt % (uint32 yMax))}

    let gaussianDiffuse (xRadius:int option) 
                        (yRadius:int option)
                        (stdDevX:float)
                        (stdDevY:float) 
                        (rando:IRando)
                        (latticeLocations:seq<LatticeLoc2d>) =
        let gaussianDiffuser (randy:IRando) = 
            Rando.normalDistInt2d stdDevX stdDevY randy

        latticeLocations |> Seq.map(fun ll-> perturb xRadius yRadius gaussianDiffuser ll rando)
        

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
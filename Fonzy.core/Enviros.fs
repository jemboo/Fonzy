namespace global

type Enviro = 
    | Empty
    | S of SorterPoolEnviro
    | A of int
    | B of float

module Enviro =

    let getOrg (e:Enviro) = 
        match e with
        | Enviro.A i -> None
        | Enviro.B i -> None
        | Enviro.S s -> None



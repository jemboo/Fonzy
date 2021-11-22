namespace global

module BasicDto =

    let fromCereal (cereal:string) =
        result {
            return!
                match cereal with
                | "true" -> Some true |> Ok
                | "false" -> Some false  |> Ok
                | "?" -> None  |> Ok
                | _ -> (sprintf "%s: not a bool option" cereal) |> Error
        }

    let toCereal (opB:bool option) =
        match opB with
        | Some true -> "true"
        | Some false -> "false"
        | None -> "?"
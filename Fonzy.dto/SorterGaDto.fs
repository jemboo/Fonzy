namespace global
open System

type SorterGenomeDto = {cat:string; value:string}
module SorterGenomeDto =
    let fromDto sorterGenomeDto = SorterGenome.Empty |> Ok
    let toDto (sorterGenome:SorterGenome) =
        {SorterGenomeDto.cat = ""; value=""}


type SorterPhenotypeDto = {cat:string; value:string}
module SorterPhenotypeDto =
    let fromDto (dto:SorterPhenotypeDto) = SorterPhenotype.Empty |> Ok
    let toDto (sorterPhenotype:SorterPhenotype) =
        {
            SorterPhenotypeDto.cat = ""; 
            value = ""
        }


type SorterTestResultsDto = {cat:string; value:string}
module SorterTestResultsDto =
    let fromDto sorterTestResultsDto = SorterTestResults.B 1 |> Ok
    let toDto (sorterTestResults:SorterTestResults) =
        {
            SorterTestResultsDto.cat = ""; 
            value=""
        }
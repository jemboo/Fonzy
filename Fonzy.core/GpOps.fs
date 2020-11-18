namespace global

type OWSel = private OWSel of (SorterPoolEnviro-> seq<Org>)
module OWSel =
    let create ff = (OWSel ff)
    let value (OWSel ff) = ff
    let fromOrgEvaluatorType omt =
        create (fun e -> Seq.empty)

type OrgWinnerSelectorType =
    | SwitchBased of MutationRate
    | StageBased of MutationRate
    
type OrgWinnerSelector = {cat:OrgWinnerSelectorType; func:OWSel}
module OrgWinnerSelector = 
    let q = None


type OBSel = private OBSel of (SorterPoolEnviro-> seq<Org>)
module OBSel =
    let create ff = (OBSel ff)
    let value (OBSel ff) = ff
    let fromOrgEvaluatorType omt =
        create (fun e -> Seq.empty)

type OrgBreederSelectorType =
    | SwitchBased of MutationRate
    | StageBased of MutationRate
    
type OrgBreederSelector = {cat:OrgBreederSelectorType; func:OBSel}
module OrgBreederSelector = 
    let q = None


type OEv = private OEv of (SorterPoolEnviro->Org->Org)
module OEv =
    let create ff = (OEv ff)
    let value (OEv ff) = ff
    let fromOrgEvaluatorType omt =
        create (fun e o -> o)

type OrgEvaluatorType =
    | SwitchBased of MutationRate
    | StageBased of MutationRate
    
type OrgEvaluator = {cat:OrgEvaluatorType; func:OEv}
module OrgEvaluator = 
    let q = None


type OMut = private OMut of (SorterPoolEnviro->Org->Org)
module OMut =
    let create ff = (OMut ff)
    let value (OMut ff) = ff
    let fromOrgMutatorType omt =
        create (fun e o -> o)

type OrgMutatorType =
    | SwitchBased of MutationRate
    | StageBased of MutationRate
    
type OrgMutator = {cat:OrgMutatorType; func:OMut}
module OrgMutator = 
    let q = None
    //let standardSwitch mr = 
    //    {SorterMutation.cat=SorterMutationCat.SwitchBased mr; 
    //    func=SM.fromSorterMutationType (SorterMutationType.Switch mr)}

    //let standardStage mr = 
    //    {SorterMutation.cat=SorterMutationCat.StageBased mr; 
    //     func=SM.fromSorterMutationType (SorterMutationType.Stage mr)}

    //let standardMutator (mutationType:SorterMutationType) =
    //    match mutationType with
    //    | SorterMutationType.Switch mr -> standardSwitch mr
    //    | SorterMutationType.Stage mr -> standardStage mr


type OrgUpdateParams = 
  {
      id: OrgUpdateParamsId;
      //breederSelector: PoolSelector2;
      //fitnessFunc: FitnessFunc;
      orgEvaluator:OrgEvaluator;
      orgMutator: OrgMutator;
      //sorterCount: SorterCount;
      //winnerSelector: PoolSelector2;
  }


type EnviroUpdateParams = 
  {
      id: EnviroUpdateParamsId;
      //breederSelector: PoolSelector2;
      //fitnessFunc: FitnessFunc;
      //sorterMutator: SorterMutation;
      //sorterCount: SorterCount;
      //winnerSelector: PoolSelector2;
  }
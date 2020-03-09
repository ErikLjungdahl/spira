module Data where

data Game = Stage StagePred [Implication ApplyPred] -- "stage " ++ Stagepred ++ ...
          | Transition Name (Implication ApplyPredOrStage)
     -- | Context Name [Pred] -- Not really needed, since they can only be used in init

data Implication a = Implication [a] [a] -- Basically -o (lolli) between the two lists

data Pred = Pred Name [Type]
          | Bwd Name [Type]

data ApplyPred = ApplyPred Pred [Var]

newtype StagePred = StagePred Name

data ApplyPredOrStage = SP StagePred  -- "stage " ++ Stagepred
                      | AP ApplyPred

data InitPred = InitPred Pred [Val]
data Initial = Initial
                    StagePred -- Initial stage
                    [InitPred]   -- Initial context.

type Name = String
newtype Type = Type Name -- Det som användaren genererar.

data Var = Var Name Type
type Val = String

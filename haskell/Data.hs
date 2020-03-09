module Data where


data Game = Stage Name [Implication] -- "stage " ++ Stagepred ++ ...
          | Transition Name Implication
          deriving (Show)
     -- | Context Name [Pred] -- Not really needed, since they can only be used in init

data Implication = Implication [Pred] [Pred] -- Basically -o (lolli) between the two lists
              deriving (Show)

data Pred = Pred Name [Type]
          | Bwd Name [Type]
          | StagePred Name
          | ApplyPred Pred [Pred] -- Pred (1)  -- Pred (2) should be a Var
          | Var Name Type
        deriving (Show)


data Initial = Initial
                    Name -- StagePred Initial stage
                    [Pred]   -- Pred that doesn't have Var or bwd -- Initial context.
          deriving (Show)

type Name = String
data Type = Type Name -- Det som användaren genererar.
          deriving (Show)

-- data Var = Var Name Type
-- type Val = String

-- data ApplyPred = ApplyPred Pred [Var]

-- newtype StagePred = StagePred Name

-- data ApplyPredOrStage = SP StagePred  -- "stage " ++ Stagepred
--                       | AP ApplyPred

-- data InitPred = InitPred Pred [Val]

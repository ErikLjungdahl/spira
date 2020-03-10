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
          | ApplyPred Pred [Var]
        deriving (Show)

data Var = Pattern Name Type
         | AVar Constructor [Var]
         deriving (Show)

data Constructor = Constructor Name [Type] Type -- ex: s nat : nat , z : nat
    deriving (Show)

data Initial = Initial
                    Name -- StagePred Initial stage
                    [Pred]   -- Pred that doesn't have Var or bwd -- Initial context.
          deriving (Show)

type Name = String
data Type = Type Name -- Det som användaren genererar.
          deriving (Show)

-- type Val = String

-- data ApplyPred = ApplyPred Pred [Var]

-- newtype StagePred = StagePred Name

-- data ApplyPredOrStage = SP StagePred  -- "stage " ++ Stagepred
--                       | AP ApplyPred

-- data InitPred = InitPred Pred [Val]

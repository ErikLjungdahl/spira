
data Game = Stage Name [Implication]
		  | Transition Name Implication
		  | Context Name [Pred]

data Implication = Implication [ApplyPred] [ApplyPred]

data Pred = Pred Name [Type]
		  | Bwd Name [Type]

data ApplyPred = ApplyPred Pred [Var]

data Var = Var Name Type

data Type = -- Det som användaren genererar.


type Name = String

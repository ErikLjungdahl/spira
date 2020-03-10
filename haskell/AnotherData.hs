
data Game = Stage Name [Implication]
		  | InteractiveStage Name [Implication]
		  | Transition Name Implication
		  | Context Name [Pred]
		  | CValue Name CType

data Implication = Implication [ApplyPred] [ApplyPred]

data Pred = Pred Name [Type]
		  | Bwd Name [Type]

data ApplyPred = ApplyPred Pred [Var]

data Var = Var Name Type

--data Type = -- Det som användaren genererar.

data CeptreType a = CType a

data Nat = Nat

data Player = Player

nat = CType Nat
player = CType Player

toString of Player
show Player = "player"

type Name = String


-- Mellan kärnan och användaren
data Move = Move

rock :: Game 
rock = CValue "rock" (CType Move)


makeCValue :: CValClass a => a -> Game
makeCValue a = CValue (getName a) (CType a)

class CValClass a where 
	getName :: a -> String


-- NU ÄR VI ANVÄNDAREN
data Piece = Piece String

instance getName Piece where
	getName (Piece str) = str


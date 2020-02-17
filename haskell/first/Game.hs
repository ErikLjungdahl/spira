module Game where


-- Datatype for creating single predicates
data Pred = Spred [String] | Tpred String [String] deriving Show


-- Write any string with \n function to the file
write :: Pred -> FilePath -> IO ()
write pr fp = do
    appendFile fp (createPreds pr)


-- Creates single predicates
predicates :: [String] -> Pred
predicates xs = Spred xs


-- Creates predicates with a type
prodicates :: String -> [String] -> Pred
prodicates t preds = Tpred t preds


-- Creates the string that are able to write to the file
createPreds :: Pred -> String
createPreds (Spred [])     = ""
createPreds (Spred (x:xs)) = x ++ " : pred.\n" ++ createPreds (Spred xs)
createPreds (Tpred t [])     = ""
createPreds (Tpred t (x:xs)) = x ++ " " ++ t ++ " : pred.\n" ++ createPreds (Tpred t xs)





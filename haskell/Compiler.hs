{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-} -- Warnings enabled


module Compiler
    ( compile
    ) where

-- Our datastructur
import Data

-- Libraries
import qualified Data.Map.Lazy as Map (insert, lookup, empty, toList)
import Data.Map.Lazy (Map)
import Control.Monad.Writer
import Data.List (intercalate, intersperse)
import Data.List.Split (splitOn)





-- TODO Add Error monad (Except)
type O a = Writer Output a
type Output = String

---- BACKEND ----
-- From our State (St) to ceptre

compile :: St -> String
compile st = snd $ runWriter (createGameFromSt st)


tell' :: String -> O ()
tell'  = \s -> tell  (s ++ "\n")
tell'' :: String -> O ()
tell'' = \s -> tell' (s ++ "\n")
tell_ :: String -> O ()
tell_ = \s -> tell (s ++ " ")

createGameFromSt :: St -> O ()
createGameFromSt st = do
    tell' "% TYPES"
    createTypes  (reverse $ types  st)
    tell' "\n% CONSTRUCTORS"
    createConsts (reverse $ consts st)
    tell' "\n% PREDS AND BWDS"
    createPreds  (reverse $ preds  st)
    tell' "\n% STAGES AND TRANSITIONS"
    createGames (columnNames st)
                (reverse $ games  st)
    tell' "\n% INITIAL"
    case initStage st of
        Nothing -> return () --TODO give error instead -- error "You must have a initial context"
        Just s -> createInit (Initial s (initialPreds st))


createTypes :: [Type] -> O ()
createTypes ts = mapM_ (\(Type name) -> tell' $ name ++ " : type.") ts



createPreds :: [Pred] -> O ()
createPreds = mapM_ (\p -> do
    createPred p
    tell' "."
    )
    where
        createPred :: Pred -> O ()
        createPred = \case
            Pred name ts -> helper name ts "pred"
            Bwd name ts -> helper name ts "bwd"
            BwdImplication b1 b2 -> do
                createPred b2
                tell "\n\t"
                tell "<- "
                createPred b1
            StagePred _ -> error "You can't initialize a StagePred, don't put it in the state"
            ApplyPred p vars -> case p of
                Bwd n ts -> do
                    appliedPred <- checkVars n ts vars
                    tell $ appliedPred
                _ -> error "You can't apply a predicate to a variable in the top level"


helper :: Name -> [Type] -> Name -> O ()
helper name ts right =
    let ts' = map (\(Type a) -> a) ts
    in tell $ name ++ ' ':(intercalate " " ts') ++ " : " ++ right

createConsts :: [Constructor] -> O ()
createConsts cs = mapM_ (\(Constructor n ts t) -> do
                            helper n ts (show t)
                            tell' ".")
                        cs

createGames :: Map Pred [Name] -> [Game] -> O ()
createGames colnames= mapM_ createGame
    where
        createGame :: Game -> O ()
        createGame = \case
            Stage n impls isInteractive -> do
                tell' $ "stage " ++ n ++ " = {"
                mapM_ (\(impl , ident) -> do
                            when isInteractive $ do
                                implicationsColNames ident impl
                            tell' ident
                            createImplication impl
                      )
                      (zip impls
                           (map (\i -> n ++ '/' : show i) ([1..] :: [Integer]))
                      )
                tell' "}"
                when isInteractive $ tell' $ "#interactive " ++ n ++ "."
                tell' ""
            Transition n impl -> do
                tell' n
                createImplication impl
                tell' ""

        createImplication :: Implication -> O ()
        createImplication (Implication ls rs) = do
                tell "\t: "
                ls' <- mapM createAppliedPred ls
                tell' $ intercalate "\n\t* " ls'
                tell "\t-o "
                rs' <- mapM createAppliedPred rs
                tell $ intercalate "\n\t* " rs'
                tell' "."

        implicationsColNames :: String -> Implication -> O ()
        implicationsColNames ident (Implication ls _) = do
            let res = foldl (predColNames) [] ls
                colns = [cn | (b,cn) <- res]
            tell' $ "%% " ++ ident ++ " " ++ (intercalate " " colns)
            return ()

        predColNames :: [(Name,String)] -> Pred -> [(Name,String)]
        predColNames context = \case
            ApplyPred p vars -> let
                helper cns =
                    foldl (\ctx vc ->
                        foldl
                            (\ctx' maybeBac ->
                                case maybeBac of
                                    Just (b,c) -> if b `elem` map fst ctx'
                                        then ctx'
                                        else  ctx' ++ [(b,c)]
                                    Nothing -> ctx'
                            ) ctx (bindingAndColname vc)
                        )
                        context
                        (zip vars cns)
                in
                case Map.lookup p colnames of
                    Just cns -> helper cns
                    Nothing -> helper $ repeat $ intersperse '/' (repeat '_')
            _ -> context
            where
                bindingAndColname :: (Var, String) -> [Maybe (Name, String)]
                bindingAndColname (v,cname) = case v of
                        Binding n _ -> [Just (n, if head cname == '_' then "_" else cname)]
                        AVar _ [] -> [Nothing]
                        AVar _ vs -> concat $ map
                            (bindingAndColname
                            ) ( zip vs (splitOn "/" cname))
-- Create the ceptre string from a Pred
checkVars:: Name -> [Type] -> [Var] -> O String
checkVars n ts vars = let
    checkVar :: Type -> Var -> O String
    checkVar t = \case
        Binding n tp ->
            if (t /= tp)
            then error "Wrong type when applying"
            else return n
        AVar (Constructor nc ts tc) vars -> do
            when (t /= tc) $ error $ "Wrong type when applying Pred " ++ n ++ " to Constructor " ++ nc ++ " with Vars " ++ show vars
            checkedvars <- zipWithM checkVar ts vars
            if checkedvars == []
            then return nc
            else let checkedvars' = intercalate " " checkedvars
                 in return $ "(" ++ nc ++ " " ++ checkedvars' ++ ")"
    in do
    when (length ts /= length vars) $ error $ "Wrong number of vars applied to a pred." ++ " Pred: " ++ n ++ ", Vars:" ++ show vars
    apreds <- zipWithM checkVar ts vars
    return $ n ++ ' ':(intercalate " " apreds)


createAppliedPred :: Pred -> O String
createAppliedPred = \case
    Pred n ts -> if ts == [] then return n
        else error $ "Predicate "++n++" needs to be applied to vars"
    Bwd n ts  -> if ts == [] then return n
        else error "bwd needs to be applied to vars"
    StagePred n -> return $ "stage " ++ n
    ApplyPred pred vars -> case pred of
        Pred n ts -> checkVars n ts vars
        Bwd  n ts -> checkVars n ts vars
        StagePred _-> error "Can't apply something to a Stage predicate"
        -- TODO Can this be supported? Does it make sense?
        ApplyPred _ _-> error "Applying something to an already applied thing isn't supported"
    Persistent p -> do
        p' <- createAppliedPred p
        return ('$':p')

    BwdImplication _ _ -> error "Can't create BwdImplication in transition/stage"


createInit :: Initial -> O ()
createInit (Initial n ps) = do
    tell "#trace _ "
    tell n
    tell "\n\t{ "
    appliedPreds <- mapM createAppliedPred ps
    tell $ intercalate "\n\t, " appliedPreds
    tell "}."

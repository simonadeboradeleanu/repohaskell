import Data.List ( union, delete )

vars :: Exp -> [IndexedVar]
vars (X x) = [x]
vars (App e1 e2) = vars e1 `union` vars e2
vars (Lam x e) = [x] `union` vars e

freeVars :: Exp -> [IndexedVar]
freeVars (X x) = [x]
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Lam x e) = delete x (freeVars e)

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement = go
  where
    go (X x)
      = X (if x == toReplace then replacement else x)
    go (App e1 e2) = App (go e1) (go e2)
    go (Lam x e)
      = Lam (if x == toReplace then replacement else x) (go e)

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement = go
  where
    go (X x)
      | x == toReplace = replacement
      | otherwise = X x
    go (App e1 e2) = App (go e1) (go e2)
    go (Lam x e)
      | x == toReplace = Lam x e
      | x `occursFree` replacement =
          let f = freshVar x (vars e `union` vars replacement)
           in Lam f (go (renameVar x f e))
      | otherwise = Lam x (go e)

normalize :: Exp -> Exp
normalize e = maybe e normalize (step e)
  where
    step (X x) = Nothing
    step (Lam x e) = Lam x <$> step e
    step (App (Lam x ex) e) = Just (substitute x e ex)
    step (App e1 e2)
      = case step e1 of
        Nothing -> App e1 <$> step e2
        Just e1' -> Just (App e1' e2)
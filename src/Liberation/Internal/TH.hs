{-#LANGUAGE TemplateHaskell, DataKinds, BlockArguments, KindSignatures#-}
module Liberation.Internal.TH where

import Language.Haskell.TH

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Kind
import Liberation.Internal.RT (RT, GetRT(..))
import Liberation.Internal.Effect (Effect)

mkEffect :: Name -> Q [Dec]
mkEffect impl = do
    (TyConI tyCon) <- reify impl
    (dataCons, tyName, tyVars, tyCxt) <- case tyCon of
            DataD    tyCxt name tvs _ cons _ -> pure (cons, name, tvs, tyCxt)
            NewtypeD tyCxt name tvs _ con  _ -> pure ([con], name, tvs, tyCxt)
            _ -> fail "Can only create effects for data or newtype definitions"
    let tyVarTys = map tyVarBndrToType tyVars

    fields <- case dataCons of
        [RecC _ fs] -> pure fs
        _ -> fail "Can only create effects for types with exactly one record data constructor"

    className <- case nameBase tyName of
        ('I':rname) -> pure $ mkName rname
        _ -> fail "Implementation name has to start with 'I'"

    esTyVar <- kindedTV <$> newName "es" <*> [t|[Data.Kind.Type]|]
    let esTy = tyVarBndrToType esTyVar


    classFields <- forM (filter (nameStartsWith '_') fields) \(mfname, _, fty) -> do
        let fname = mkName (drop 1 (nameBase mfname))
        sigD fname (removeEs fty esTyVar)

    let effectCxt = AppT (AppT (ConT (''Effect)) (foldl AppT (ConT tyName) (map tyVarBndrToType tyVars))) (esTy)

    sequence [
            classD (pure (tyCxt <> [effectCxt])) className (tyVars <> [esTyVar]) [] (map pure classFields)
        ,   instanceD
                (sequence [[t|$(conT ''MonadIO) (RT $(pure esTy))|], [t|$(conT ''GetRT) $(foldl appT (conT tyName) (map pure tyVarTys)) $(pure esTy)|]])
                (appT (foldl appT (conT className) (map pure tyVarTys)) (pure $ esTy))
                (flip map (filter (nameStartsWith '_') fields) \(ifname, _, fty) -> do
                    let fname = mkName (drop 1 (nameBase ifname))
                    vname <- newName "r"
                    params <- replicateM (arity fty) (newName "p")
                    --params <- replicateM 1 (newName "p")

                    let body = lamE (map varP params) [|$(varE 'getRT) >>= \ $(varP vname) -> $(appsE (map varE $ [ifname, vname] <> params))|]
                    head <$> [d|$(varP fname) = $(body)|])
        ]

arity :: Type -> Int
arity (ForallT _ _ t) = arity t
arity (ForallVisT _ t) = arity t
arity (AppT (AppT ArrowT _) t) = 1 + arity t
arity _ = 0

removeEs :: Type -> TyVarBndr -> Q Type
removeEs (ForallT tvbndr tcxt ty) esTyVar = pure $
    ForallT
      (filter (not . tvIsCalled "es") tvbndr)
      (filter (not . includesTvWithName "es") tcxt)
      (replaceTvWithName "es" (tyVarBndrToType esTyVar) ty)
removeEs ty _ = pure ty

tvIsCalled :: String -> TyVarBndr -> Bool
tvIsCalled name (PlainTV tvName) = nameBase tvName == name
tvIsCalled name (KindedTV tvName _) = nameBase tvName == name

includesTvWithName :: String -> Pred -> Bool
includesTvWithName name (VarT n) = nameBase n == name
includesTvWithName name (AppT t1 t2) = includesTvWithName name t1 || includesTvWithName name t2
includesTvWithName _ _ = False

replaceTvWithName :: String -> Type -> Type -> Type
replaceTvWithName name repl (VarT n) | nameBase n == name = repl
replaceTvWithName name repl (AppT t1 t2) = AppT (replaceTvWithName name repl t1) (replaceTvWithName name repl t2)
replaceTvWithName _ _ t = t

tyVarBndrToType :: TyVarBndr -> Type
tyVarBndrToType (PlainTV n) = VarT n
tyVarBndrToType (KindedTV n _) = VarT n

nameStartsWith :: Char -> (Name, Bang, Type) -> Bool
nameStartsWith c (n, _, _) = head (nameBase n) == c


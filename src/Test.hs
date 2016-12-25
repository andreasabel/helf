module Test where

import Prelude hiding (pi,abs)
import Data.Char -- testing

import qualified Abstract as A
import Closures

-- polymorphic identity
hashString = fromIntegral . foldr f 0
      where f c m = ord c + (m * 128) `rem` 1500007

hash :: String -> A.Name
hash s = A.Name (hashString s) s

var' x   = A.Ident $ A.Var $ hash x
abs x e = A.Lam (hash x) Nothing e
app     = A.App
ty = A.Typ
pi x = A.Pi (Just $ hash x)

eid = abs "A" $ abs "x" $ var' "x"
tid = pi "A" ty $ pi "x" (var' "A") $ var' "A"

arrow a b = A.Pi Nothing a b

tnat = pi "A" ty $
         pi "zero" (var' "A") $
         pi "suc"  (var' "A" `arrow` var' "A") $
           var' "A"

ezero  = abs "A" $ abs "zero" $ abs "suc" $ var' "zero"
-- problem: esuc is not a nf
esuc n = abs "A" $ abs "zero" $ abs "suc" $ var' "suc" `app`
          (n `app` var' "A" `app` var' "zero" `app` var' "suc")

enat e =  abs "A" $ abs "zero" $ abs "suc" $ e
enats = map enat $ iterate (app (var' "suc")) (var' "zero")
e2 = enats !! 2

rid = runCheck eid tid

success = [(eid,tid),(ezero,tnat),(e2,tnat)] -- ,(Sort Type,Sort Kind)]
failure = [(tid,tid)]

runsuccs = map (uncurry runCheck) success
runtests = map (uncurry runCheck) (success ++ failure)

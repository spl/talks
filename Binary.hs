{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Binary where

import Generics.Deriving
import Data.Binary (Put, Get, putWord8, getWord8)
import Control.Applicative

class Binary t where
  default put :: (Generic t, GBinary (Rep t)) => t -> Put

  put :: t -> Put
  put = put_default

  default get :: (Generic t, GBinary (Rep t)) => Get t

  get :: Get t
  get = get_default

class GBinary f where
  gput :: f a -> Put
  gget :: Get (f a)

instance GBinary U1 where
  gput U1  = return ()
  gget     = return U1

instance (Binary a) => GBinary (K1 i a) where
  gput (K1 x)  = put x
  gget         = K1 <$> get

instance (GBinary a) => GBinary (M1 i c a) where
  gput (M1 x)  = gput x
  gget         = M1 <$> gget

instance (GBinary f, GBinary g) => GBinary (f :+: g) where
  gput (L1 x)  = putWord8 0 >> gput x
  gput (R1 y)  = putWord8 1 >> gput y

  gget = do  w <- getWord8
             case w of  0  -> L1  <$> gget
                        _  -> R1  <$> gget

instance (GBinary f, GBinary g) => GBinary (f :*: g) where
  gput (x :*: y)  = gput x >> gput y
  gget            = (:*:) <$> gget <*> gget

put_default :: (Generic t, GBinary (Rep t)) => t -> Put
put_default = gput . from

get_default :: (Generic t, GBinary (Rep t)) => Get t
get_default = to <$> gget
\end{code}

\begin{code}
data Exp  =  Var String
          |  Lam String Exp
          |  App Exp Exp
--          deriving Generic

-- Derived instances

instance Generic Exp where
  type Rep Exp =
    D1 D1Exp  (    C1 C1_0Exp (S1 S1_0_0Exp (Rec0 String))
              :+:  C1 C1_1Exp (S1 S1_1_0Exp (Rec0 String) :*: S1 S1_1_1Exp (Rec0 Exp))
              :+:  C1 C1_2Exp (S1 S1_2_0Exp (Rec0 Exp) :*: S1 S1_2_1Exp (Rec0 Exp)))
  from (Var g1_aCY)
    = M1 (L1 (M1 (M1 (K1 g1_aCY))))
  from (Lam g1_aCZ g2_aD0)
    = M1 (R1 (L1 (M1 ((:*:) (M1 (K1 g1_aCZ)) (M1 (K1 g2_aD0))))))
  from (App g1_aD1 g2_aD2)
    = M1 (R1 (R1 (M1 ((:*:) (M1 (K1 g1_aD1)) (M1 (K1 g2_aD2))))))

  to (M1 (L1 (M1 (M1 (K1 g1_aD3)))))
    = Var g1_aD3
  to (M1 (R1 (L1 (M1 ((:*:) (M1 (K1 g1_aD4)) (M1 (K1 g2_aD5)))))))
    = Lam g1_aD4 g2_aD5
  to (M1 (R1 (R1 (M1 ((:*:) (M1 (K1 g1_aD6)) (M1 (K1 g2_aD7)))))))
    = App g1_aD6 g2_aD7

instance Datatype D1Exp where
  datatypeName _ = "Exp"
  moduleName _ = "Binary"

instance Constructor C1_0Exp where
  conName _ = "Var"

instance Constructor C1_1Exp where
  conName _ = "Lam"

instance Constructor C1_2Exp where
  conName _ = "App"

data D1Exp
data C1_0Exp
data C1_1Exp
data C1_2Exp
data S1_0_0Exp
data S1_1_0Exp
data S1_1_1Exp
data S1_2_0Exp
data S1_2_1Exp


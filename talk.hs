{-# LINE 60 "talk.lhs" #-}
{-#  LANGUAGE TypeOperators  #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE MultiParamTypeClasses  #-}
{-#  LANGUAGE FlexibleContexts  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE OverlappingInstances  #-}
{-#  LANGUAGE UndecidableInstances  #-}
module Talk where
import Prelude hiding (sum, any)

test =  test1
     && test2
     && test3
     && test4
     && test5
     && test6
     && test7
     && test8
     && test9
     && test10
{-# LINE 204 "talk.lhs" #-}
data Tree aa = Tip | Leaf aa | Node Int (Tree aa) (Tree aa)
{-# LINE 208 "talk.lhs" #-}
deriving instance (Eq aa) => Eq (Tree aa)
deriving instance (Show aa) => Show (Tree aa)
{-# LINE 243 "talk.lhs" #-}
type TreeS' aa = Either () (Either aa (Int, (Tree aa, Tree aa)))
{-# LINE 258 "talk.lhs" #-}
data UnitT      = Unit          --  ()

data aa :*: bb  = aa ::*:: bb   --  (a, b)

data aa :+: bb  = L aa | R bb   --  Either a b
{-# LINE 266 "talk.lhs" #-}
infixr 5 :+:
infixr 6 :*:
infixr 6 ::*::
deriving instance Enum UnitT
deriving instance Eq UnitT
deriving instance Ord UnitT
deriving instance Read UnitT
deriving instance Show UnitT
deriving instance (Eq aa, Eq bb) => Eq (aa :+: bb)
deriving instance (Ord aa, Ord bb) => Ord (aa :+: bb)
deriving instance (Read aa, Read bb) => Read (aa :+: bb)
deriving instance (Show aa, Show bb) => Show (aa :+: bb)
deriving instance (Eq aa, Eq bb) => Eq (aa :*: bb)
deriving instance (Ord aa, Ord bb) => Ord (aa :*: bb)
deriving instance (Read aa, Read bb) => Read (aa :*: bb)
deriving instance (Show aa, Show bb) => Show (aa :*: bb)
{-# LINE 290 "talk.lhs" #-}
type TreeS aa = UnitT :+: aa :+: Int :*: Tree aa :*: Tree aa
{-# LINE 298 "talk.lhs" #-}
data ConDescrT   = ConDescr  

data TypeDescrT  = TypeDescr  
{-# LINE 305 "talk.lhs" #-}
deriving instance Eq ConDescrT
deriving instance Ord ConDescrT
deriving instance Read ConDescrT
deriving instance Show ConDescrT
deriving instance Eq TypeDescrT
deriving instance Ord TypeDescrT
deriving instance Read TypeDescrT
deriving instance Show TypeDescrT
{-# LINE 330 "talk.lhs" #-}
data EPT dd rr = EP { from :: (dd -> rr), to :: (rr -> dd) }
{-# LINE 337 "talk.lhs" #-}
epTree :: EPT (Tree aa) (TreeS aa)
epTree = EP fromTree toTree

  where  fromTree  Tip             = L Unit
         fromTree  (Leaf a)        = R (L a)
         fromTree  (Node i t1 t2)  = R (R (i ::*:: t1 ::*:: t2))

         toTree  (L Unit)                       = Tip
         toTree  (R (L a))                      = Leaf a
         toTree  (R (R (i ::*:: t1 ::*:: t2)))  = Node i t1 t2
{-# LINE 384 "talk.lhs" #-}
rTree :: (Generic gg, Rep gg aa, Rep gg Int, Rep gg (Tree aa)) => gg (Tree aa)
rTree = rtype  (TypeDescr  ) epTree
               (  rcon (ConDescr  )  runit  `rsum`
                  rcon (ConDescr  )  rep    `rsum`
                  rcon (ConDescr  )  (rep `rprod` rep `rprod` rep))
{-# LINE 404 "talk.lhs" #-}
class Rep gg aa where
  rep :: gg aa

instance  (Generic gg, Rep gg aa, Rep gg Int, Rep gg (Tree aa)) =>
          Rep gg (Tree aa) where
  rep = rTree
{-# LINE 417 "talk.lhs" #-}
instance (Generic gg) => Rep gg Int where
  rep = rint
 
{-# LINE 423 "talk.lhs" #-}
instance (Generic gg) => Rep gg Char where
  rep = rchar
{-# LINE 465 "talk.lhs" #-}
class Generic gg where
  rconstant  :: (Enum aa, Eq aa, Ord aa, Read aa, Show aa) => gg aa
  rint       :: gg Int
  rinteger   :: gg Integer
  rfloat     :: gg Float
  rdouble    :: gg Double
  rchar      :: gg Char
  runit      :: gg UnitT
  rsum       :: gg aa -> gg bb -> gg (aa :+: bb)
  rprod      :: gg aa -> gg bb -> gg (aa :*: bb)
  rcon       :: ConDescrT -> gg aa -> gg aa
  rtype      :: TypeDescrT -> EPT bb aa -> gg aa -> gg bb
{-# LINE 480 "talk.lhs" #-}
  rint      = rconstant
  rinteger  = rconstant
  rfloat    = rconstant
  rdouble   = rconstant
  rchar     = rconstant
  runit     = rconstant
  rcon      = const id

infixr 6 `rprod`
infixr 5 `rsum`
{-# LINE 511 "talk.lhs" #-}
newtype EmptyT aa = Empty { selEmpty :: aa }
{-# LINE 528 "talk.lhs" #-}
instance Generic EmptyT where
  rconstant                 = error "Should not be called!"
  rint                      = Empty 0
  rinteger                  = Empty 0
  rfloat                    = Empty 0
  rdouble                   = Empty 0
  rchar                     = Empty '\NUL'
  runit                     = Empty Unit
  rsum              ra  rb  = Empty (L (selEmpty ra))
  rprod             ra  rb  = Empty (selEmpty ra ::*:: selEmpty rb)
  rcon      cd      ra      = Empty (selEmpty ra)
  rtype     td  ep  ra      = Empty (to ep (selEmpty ra))
{-# LINE 552 "talk.lhs" #-}
empty :: (Rep EmptyT aa) => aa
empty = selEmpty rep
{-# LINE 564 "talk.lhs" #-}
test1 = (empty :: Tree Int) == Tip
{-# LINE 696 "talk.lhs" #-}
class FRep gg ff where
  frep :: gg aa -> gg (ff aa)
{-# LINE 709 "talk.lhs" #-}
instance (Generic gg) => FRep gg Tree where
  frep ra =
    rtype  (TypeDescr  ) epTree
           (  rcon (ConDescr  )  runit  `rsum`
              rcon (ConDescr  )  ra     `rsum`
              rcon (ConDescr  )  (rint `rprod` frep ra `rprod` frep ra))
{-# LINE 747 "talk.lhs" #-}
newtype CrushT bb aa = Crush { selCrush :: aa -> bb -> bb }
{-# LINE 769 "talk.lhs" #-}
instance Generic (CrushT bb) where
{-# LINE 778 "talk.lhs" #-}
  rconstant         = Crush (const id)
  rcon   cd         = Crush . selCrush
  rtype  td  ep ra  = Crush (selCrush ra . from ep)
{-# LINE 788 "talk.lhs" #-}
  rsum ra rb = Crush go
    where  go (L a)  = selCrush ra a
           go (R b)  = selCrush rb b
{-# LINE 802 "talk.lhs" #-}
  rprod ra rb = Crush go
    where  go (a ::*:: b) = selCrush ra a . selCrush rb b
{-# LINE 824 "talk.lhs" #-}
data Assoc  =  AssocLeft
            |  AssocRight

newtype CrushT2 bb aa = Crush2 { selCrush2 :: Assoc -> aa -> bb -> bb }
{-# LINE 835 "talk.lhs" #-}
instance Generic (CrushT2 bb) where
   
  rprod ra rb = Crush2 go
    where
      go s@AssocLeft   (a ::*:: b)  = selCrush2 rb s b . selCrush2 ra s a
      go s@AssocRight  (a ::*:: b)  = selCrush2 ra s a . selCrush2 rb s b
   
{-# LINE 845 "talk.lhs" #-}
  rconstant         = Crush2 (\_ _ -> id)
  rcon   cd         = Crush2 . selCrush2
  rtype  td  ep ra  = Crush2 (\s -> selCrush2 ra s . from ep)
  rsum ra rb = Crush2 go
    where  go s (L a)  = selCrush2 ra s a
           go s (R b)  = selCrush2 rb s b
{-# LINE 927 "talk.lhs" #-}
crush ::  (FRep (CrushT2 bb) ff) =>
          Assoc -> (aa -> bb -> bb) -> bb -> ff aa -> bb
crush s f z x = selCrush2 (frep (Crush2 (const f))) s x z
{-# LINE 937 "talk.lhs" #-}
crushl, crushr ::  (FRep (CrushT2 bb) ff) =>
                   (aa -> bb -> bb) -> bb -> ff aa -> bb
crushl = crush AssocLeft

crushr = crush AssocRight
{-# LINE 963 "talk.lhs" #-}
flattenr :: (FRep (CrushT2 [a]) f) => f a -> [a]
flattenr = crushr (:) []

test2 =  flattenr (Node 2 (Leaf "Hi") (Leaf "London"))
         == ["Hi","London"]
{-# LINE 977 "talk.lhs" #-}
flattenl :: (FRep (CrushT2 [a]) f) => f a -> [a]
flattenl = crushl (:) []

test3 =  flattenl (Node 2009 (Leaf 7) (Leaf 9)) == [9,7]
{-# LINE 997 "talk.lhs" #-}
sum :: (Num a, FRep (CrushT2 a) f) => f a -> a
sum = crushr (+) 0

test4 = sum (Node 4 (Leaf 40) (Leaf 2)) == 42
{-# LINE 1010 "talk.lhs" #-}
any :: (FRep (CrushT2 Bool) f) => (a -> Bool) -> f a -> Bool
any p = crushr (\x b -> b || p x) False

test5 = any (>2) (Node 5 (Leaf 0) (Leaf 1)) == False
{-# LINE 1036 "talk.lhs" #-}
instance Rep EmptyT (Tree Char) where
  rep = Empty (Leaf empty)

test6 = empty == Leaf '\NUL'
{-# LINE 1099 "talk.lhs" #-}
newtype CollectT bb aa = Collect { selCollect :: aa -> [bb] }
{-# LINE 1119 "talk.lhs" #-}
instance Generic (CollectT b) where
  rconstant            = Collect (const [])
  rcon   cd      ra    = Collect (selCollect ra)
  rtype  td  ep  ra    = Collect (selCollect ra . from ep)
{-# LINE 1139 "talk.lhs" #-}
  rsum ra rb = Collect go
    where  go (L a)  = selCollect ra a
           go (R b)  = selCollect rb b
{-# LINE 1150 "talk.lhs" #-}
  rprod ra rb = Collect go
    where  go (a ::*:: b) = selCollect ra a ++ selCollect rb b
{-# LINE 1159 "talk.lhs" #-}
collect :: (Rep (CollectT bb) aa) => aa -> [bb]
collect = selCollect rep
{-# LINE 1172 "talk.lhs" #-}
instance Rep (CollectT Int) Int where
  rep = Collect (:[])
{-# LINE 1177 "talk.lhs" #-}
instance Rep (CollectT Char) Char where
  rep = Collect (:[])
{-# LINE 1187 "talk.lhs" #-}
instance Rep (CollectT (Tree aa)) (Tree aa) where
  rep = Collect (:[])
{-# LINE 1203 "talk.lhs" #-}
val1 = Node 88 (Leaf 'a') (Leaf 'b')

test7 =  collect val1 == "ab"

test8 = collect val1 == [88 :: Int]
{-# LINE 1217 "talk.lhs" #-}
val2 :: Tree Int
val2 = (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))

test9 =  collect val2 == [1,2,3,4,5 :: Int]

test10 =  collect val2 == [val2]

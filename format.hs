{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

--------------------------------------------------------------------------------

(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend

--------------------------------------------------------------------------------

data Token
  = VaridT Text
  | ConidT Text

data Lhs
  = PlainL Text
  | PrimeL Text
  | SupL Text Text
  | SubL Text Text
  | SubPrimeL Text Text

data Rhs
  = PlainR Token
  | PrimeR Token
  | SupR Token Text
  | SubR Token Text
  | SubPrimeR Token Text

data Format
  = Format Lhs Rhs

--------------------------------------------------------------------------------

data Level
  = Sup
  | Sub

level :: Level -> Text
level Sup = "^"
level Sub = "_"

levelText :: Text -> Level -> Text -> Text
levelText b l s = mconcat ["{", b, "}", level l, "{", s, "}"]

levelText2 :: Text -> Level -> Text -> Level -> Text -> Text
levelText2 b l1 s1 l2 s2 =
  mconcat ["{", b, "}", level l1, "{", s1, "}", level l2, "{", s2, "}"]

--------------------------------------------------------------------------------

class ToText a where
  toText :: a -> Text

printText :: (ToText a) => a -> IO ()
printText = TIO.putStr . toText

instance (ToText a) => ToText [a] where
  toText = T.unlines . map toText

instance ToText Char where
  toText = T.singleton

instance ToText Int where
  toText = T.pack . show

instance ToText Token where
  toText (VaridT t)  = "\\Varid{" <> t <> "}"
  toText (ConidT t)  = "\\Conid{" <> t <> "}"

instance ToText Lhs where
  toText (PlainL t) = t
  toText (PrimeL t) = t <> "'"
  toText (SupL t1 t2) = t1 <> "'" <> t2
  toText (SubL t1 t2) = t1 <> "_" <> t2
  toText (SubPrimeL t1 t2) = t1 <> "_" <> t2 <> "'"

prime = T.pack "\\prime"

copyToken :: Token -> Text -> Text
copyToken tok t = toText $ case tok of
  VaridT _ -> VaridT t
  ConidT _ -> ConidT t

instance ToText Rhs where
  toText (PlainR tok) = toText tok
  toText (PrimeR tok) = levelText (toText tok) Sup (copyToken tok prime)
  toText (SupR tok s) = levelText (toText tok) Sup (copyToken tok s)
  toText (SubR tok s) = levelText (toText tok) Sub (copyToken tok s)
  toText (SubPrimeR tok s) = levelText2 (toText tok) Sub (copyToken tok s) Sup (copyToken tok prime)

instance ToText Format where
  toText (Format lhs rhs) = "%format " <> toText lhs <> " = \"" <> toText rhs <> "\""

--------------------------------------------------------------------------------

mkFormats1 mkL mkR mkToken bs = [ Format (mkL l) (mkR (mkToken r)) | (l, r) <- bs ]
mkFormats2 mkL mkR mkToken bs ss = [ Format (mkL l sl) (mkR (mkToken r) sr) | (l, r) <- bs, (sl, sr) <- ss ]

mkPlainFormats = mkFormats1 PlainL PlainR
mkPrimeFormats = mkFormats1 PrimeL PrimeR
mkSupFormats = mkFormats2 SupL SupR
mkSubFormats = mkFormats2 SubL SubR
mkSubPrimeFormats = mkFormats2 SubPrimeL SubPrimeR

formatters1 = [mkPlainFormats, mkPrimeFormats]
formatters2 = [mkSupFormats, mkSubFormats, mkSubPrimeFormats]

--------------------------------------------------------------------------------

numbers = map toText [1..5 :: Int]

lowerAlpha = map toText ['a'..'z']
upperAlpha = map toText ['A'..'Z']
alpha = lowerAlpha <> upperAlpha

lowerGreek = map T.pack ["alpha","beta","gamma","delta","epsilon","varepsilon","zeta","eta","theta","vartheta","iota","kappa","varkappa","lambda","mu","nu","xi","pi","varpi","rho","varrho","sigma","varsigma","tau","upsilon","phi","varphi","chi","psi","omega"]
upperGreek = map T.pack ["Gamma","Delta","Lambda","Phi","Pi","Psi","Sigma","Theta","Upsilon","Xi","Omega"]
greek = lowerGreek <> upperGreek

haskellTypes = map T.pack ["Char","Int","Integer","Float","Double","List","Bool","Maybe","Either","Ordering"]
haskellClasses = map T.pack ["Show","Read","Eq","Enum","Bounded","Num","Monad","Functor","Applicative"]

other = map T.pack
  ["Shape","Circle","Rect","circle","rect","drawShape","drawShapes","head"
  ,"Nil","Cons","ex","Vec","fromList","zipWith","zipWithToVec"
  ,"DInt","DInteger","Tree","Tip","Bin","in","out","In","Fix"]

--------------------------------------------------------------------------------

toCommand = T.cons '\\' 

varidBasesL = alpha <> greek               <> haskellTypes <> haskellClasses <> other
varidBasesR = alpha <> map toCommand greek <> haskellTypes <> haskellClasses <> other

varidIndexesL = alpha <> numbers
varidIndexesR = alpha <> numbers

--------------------------------------------------------------------------------

varidBaseFormats    = concatMap (($ zip varidBasesL varidBasesR) . ($ VaridT)) formatters1
varidIndexedFormats = concatMap (($ zip varidIndexesL varidIndexesR) . ($ zip varidBasesL varidBasesR) . ($ VaridT)) formatters2

--------------------------------------------------------------------------------

main = do
  putStrLn "%if style /= newcode"
  mapM_ printText
    [ varidBaseFormats
    , varidIndexedFormats
    ]
  putStrLn "%endif"


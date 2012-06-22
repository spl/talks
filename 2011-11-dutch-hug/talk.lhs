\documentclass{beamer}

%-------------------------------------------------------------------------------
% C Preprocessor Directives

#undef DO_PAUSES

#include "pause.h"

%-------------------------------------------------------------------------------
% Packages

%-------------------------------------------------------------------------------
% Formatting (includes)

%include talk.fmt
%include forall.fmt

%-------------------------------------------------------------------------------
% Formatting (specific)

%if style /= newcode

%format =~ = "\cong "

%format unL_1
%format unR_1
%format unK_1
%format unM_1
%format Par_1
%format Rec_1
%format unPar_1
%format unRec_1
%format Par_0
%format Rec_0
%format Generic_1
%format Rep_1
%format from_1
%format to_1
%format Comp_1
%format unComp_1

%format U1 = U_1
%format L1 = L_1
%format R1 = R_1
%format K1 = K_1
%format M1 = M_1
%format D1 = D_1
%format C1 = C_1
%format S1 = S_1
%format unL1 = unL_1
%format unR1 = unR_1
%format unK1 = unK_1
%format unM1 = unM_1
%format Par1 = Par_1
%format Rec1 = Rec_1
%format unPar1 = unPar_1
%format unRec1 = unRec_1
%format Par0 = Par_0
%format Rec0 = Rec_0
%format Generic1 = Generic_1
%format Rep1 = Rep_1
%format from1 = from_1
%format to1 = to_1
%format Comp1 = Comp_1
%format unComp1 = unComp_1

%format List1 = List
%format List2 = List
%format List3 = List

%format Nil1 = Nil
%format Nil2 = Nil
%format Nil3 = Nil

%format Cons1 = Cons
%format Cons2 = Cons
%format Cons3 = Cons

%format O = 0
%format I = 1

%endif

%if style == newcode

%format openq =
%format closeq =

%format clubsuit = ""♣""
%format diamondsuit = ""♦""
%format heartsuit = ""♥""
%format spadesuit = ""♠""

%else

%format openq = "\text{``}"
%format closeq = "\text{''''}"

%format clubsuit = "\clubsuit"
%format diamondsuit = "\diamondsuit"
%format heartsuit = "\heartsuit"
%format spadesuit = "\spadesuit"

%endif

%-------------------------------------------------------------------------------
% Commands

%-------------------------------------------------------------------------------
% Titles

%-------------------------------------------------------------------------------
% Code

%if style == newcode
\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
module Talk where
import qualified Generics.Deriving as G
import Data.Binary (Put, Get, putWord8, getWord8)
import Control.Applicative

infixr 5 :+:
infixr 6 :*:
data List1 a = Nil1 | Cons1 a (List1 a)
data List2 a = Nil2 | Cons2 a (List2 a)
deriving instance (Show a) => Show (List1 a)
deriving instance (Show a) => Show (List2 a)
deriving instance Functor List2
\end{code}
%endif

%-------------------------------------------------------------------------------
% Title

\title[Generic Deriving]{Generic Deriving \\ in GHC 7.2}

\author{Sean Leather}

\institute{Utrecht University}

\date[2011-10-18]{8 November 2011}

%-------------------------------------------------------------------------------

\begin{document}

%-------------------------------------------------------------------------------
\begin{frame}

\titlepage

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{|deriving|}

You're familiar with the |deriving| mechanism in Haskell:

\begin{code}
data Suit = Club | Diamond | Heart | Spade
  (RED(deriving)) (Eq, Ord, Enum, Bounded, Read {-, Show -})
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Not |deriving|}

\begin{code}
instance Show Suit where
  show Club      = openq clubsuit closeq
  show Diamond   = openq diamondsuit closeq
  show Heart     = openq heartsuit closeq
  show Spade     = openq spadesuit closeq
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Problems with |deriving|}

PAUSE
\begin{itemize}INCREMENT
\item Specification is largely informal
\begin{itemize}
\item Difficult to validate an implementation against the specification
\end{itemize}
\item Restricted to |Eq|, |Ord|, |Enum|, |Bounded|, |Read|, and |Show|
\begin{itemize}
\item Cannot reuse the |deriving| mechanism for other purposes
\end{itemize}
\item Mechanism is built into the compiler
\begin{itemize}
\item More work to maintain compiler than library
\item Duplicated functionality for each derived class
\item Difficult to share implementations between compilers
\end{itemize}
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Alternatives to |deriving|}

PAUSE
Use generic programming libraries!

PAUSE
(of course)

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Problems with GP}

PAUSE
\begin{itemize}INCREMENT
\item Not built into compiler
\begin{itemize}
\item Requires boilerplate or Template Haskell to use library
\end{itemize}
\item Requires additional libraries
\begin{itemize}
\item Nonstandard libraries are often not used
\end{itemize}
\item Some libraries are difficult to understand/use
\begin{itemize}
\item Complicated types and functions
\end{itemize}
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Hello, Generic Deriving (GD)}

PAUSE
\begin{itemize}INCREMENT
\item A library: \verb=generic-deriving= on Hackage
\item An implementation of |deriving| |Eq|, |Ord|, etc.
\item Language extensions in GHC 7.2
\begin{itemize}
\item Default implementation for type class methods
\item |deriving| the instance for \verb=generic-deriving=
\end{itemize}
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{How Does GD Work?}
%if style /= newcode
%format Eq1 = Eq
%format === = ==
%endif

\begin{spec}
class Eq1 a where 
  (===) :: a -> a -> Bool
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{How Does GD Work?}

\begin{spec}
{-# LANGUAGE DefaultSignatures #-}
\end{spec}

PAUSE
\begin{code}
class Eq1 a where 
  (===) :: a -> a -> Bool

  (RED(default)) (===) :: ((BLUE(Generic)) a, (BLUE(GEq)) ((BLUE(Rep)) a)) => a -> a -> Bool
  x === y = geq (from x) (from y)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{How Does GD Work?}
%if style /= newcode
%format G.Generic = Generic
%format G.GEq = Eq1
%format `G.geq` = ===
%endif

\begin{spec}
{-# LANGUAGE DeriveGeneric #-}
\end{spec}

PAUSE
\begin{spec}
import Generics.Deriving
\end{spec}

PAUSE
\begin{code}
data Exp  =  Var String
          |  Lam String Exp
          |  App Exp Exp
          deriving (BLUE(G.Generic))
\end{code}

PAUSE
\begin{code}
instance G.GEq Exp
\end{code}

PAUSE
\begin{code}
test = Lam "x" (Var "x") `G.geq` Lam "y" (Var "y")
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Behind the Scenes}

\begin{itemize}INCREMENT
\item What are |Generic|, |Rep|, and |GEq|?
\item How does |deriving Generic| allow us to use the |Eq1| class?
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{|Generic|}

\begin{code}
class Generic a where
  type Rep a  :: * -> *
  from        :: a -> Rep a x
  to          :: Rep a x -> a
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Isomorphisms}

\begin{columns}[t]

\column{.44\textwidth}
ONSLIDE(1)
\textbf{Haskell Type}

ONSLIDE(2)
|()|

ONSLIDE(3)
|(a, b)|

ONSLIDE(4)
|Either a b|

ONSLIDE(5)
|Maybe a|

ONSLIDE(6)
|[a]|

ONSLIDE(7)
\begin{spec}
data Exp  =  Var  String
          |  Lam  String  Exp
          |  App  Exp     Exp
\end{spec}

\column{.04\textwidth}
ONSLIDE(1)
\centering
\(\cong\)

\column{.44\textwidth}
ONSLIDE(1)
\textbf{Representation}

ONSLIDE(2)
|()|

ONSLIDE(3)
|(a, b)|

ONSLIDE(4)
|Either a b|

ONSLIDE(5)
|Either () a|

ONSLIDE(6)
|Either () (a, [a])|

ONSLIDE(7)
\begin{spec}
   Either     String
(  Either  (  String  , Exp)
           (  Exp     , Exp))
\end{spec}

\end{columns}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Representation Types}

Unit: |()|

PAUSE
\begin{code}
data U1 p = U1
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Representation Types}

Binary product (i.e.\ pair): |(a, b)|

PAUSE
\begin{code}
data (f :*: g) p = f p :*: g p
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Representation Types}

Binary sum (i.e.\ coproduct, alternatives): |Either a b|

PAUSE
\begin{code}
data (f :+: g) p = L1 { unL1 :: f p } | R1 { unR1 :: g p }
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Representation Types}

Constant types: primitives, other types without representation

PAUSE
\begin{code}
newtype K1 i a p = K1 { unK1 :: a }
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Representation Types}

Metadata: constructor and datatype names, associativity, fixity

PAUSE
\begin{code}
newtype M1 i c f p = M1 { unM1 :: f p }
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Representation Type Synonyms}

\begin{columns}

\column{.48\textwidth}

Parameter type
\begin{code}
type Par0 = K1 P
data P
\end{code}

ONSLIDE(2)
Recursive type
\begin{code}
type Rec0  = K1 R
data R
\end{code}

ONSLIDE(3)
Note that these aren't that useful for most generic functions.

\column{.48\textwidth}

ONSLIDE(4)
Datatype tag
\begin{code}
type D1  = M1 D
data D
\end{code}

ONSLIDE(5)
Constructor tag
\begin{code}
type C1  = M1 C
data C
\end{code}

ONSLIDE(6)
Selector (label) tag
\begin{code}
type S1  = M1 S
data S
\end{code}

\end{columns}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Simplified Representation of Lists}

\begin{spec}
class Generic a where
  type Rep a  :: * -> *
  from        :: a -> Rep a x
  to          :: Rep a x -> a
\end{spec}

PAUSE
\begin{spec}
instance Generic [a] where
  type Rep [a] = U1 :+: Par0 a :*: Rec0 [a]

  from []        = L1  U1
  from (x : xs)  = R1  (K1 x :*: K1 xs)

  to (L1  U1)                = []
  to (R1  (K1 x :*: K1 xs))  = x : xs
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{``Full'' Representation of Lists}

\begin{code}
instance Generic [a] where
  type Rep [a] = D1 (BLUE(DL)) (  C1 (BLUE(CN)) (S1 (BLUE(SU)) ((RED(U1)))) (RED(:+:))
                                  C1 (BLUE(CC)) (S1 (BLUE(SP)) ((RED(Par0 a))) (RED(:*:)) S1 (BLUE(SR)) ((RED(Rec0 [a])))))

  from []        = M1 (L1  (M1 (M1 U1)))
  from (x : xs)  = M1 (R1  (M1 (M1 (K1 x) :*: M1 (K1 xs))))

  to (M1 (L1  (M1 (M1 U1))))                     = []
  to (M1 (R1  (M1 (M1 (K1 x) :*: M1 (K1 xs)))))  = x : xs
\end{code}

PAUSE
\begin{columns}[t]

\column{.3\textwidth}

\begin{code}
data (BLUE(DL))
\end{code}

\column{.3\textwidth}

\begin{code}
data (BLUE(CN))
data (BLUE(CC))
\end{code}

\column{.3\textwidth}

\begin{code}
data (BLUE(SU))
data (BLUE(SP))
data (BLUE(SR))
\end{code}

\end{columns}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic Function}

\begin{code}
class GEq f where
  geq :: f a -> f a -> Bool
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic Function}

\begin{code}
instance GEq U1 where
  geq _ _ = True
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic Function}

\begin{code}
instance (Eq1 a) => GEq (K1 i a) where
  geq (K1 a) (K1 b) = a === b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic Function}

\begin{code}
instance (GEq a) => GEq (M1 i c a) where
  geq (M1 a) (M1 b) = geq a b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic Function}

\begin{code}
instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic Function}

\begin{code}
instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a) (L1 b)  = geq a b
  geq (R1 a) (R1 b)  = geq a b
  geq _      _       = False
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Design of Generic Deriving}

PAUSE
Goals:
\begin{enumerate}INCREMENT

\item Support |deriving Functor|

\item Support |deriving| for |Eq|, |Ord|, |Enum|, |Bounded|, |Show|, and |Read|

\item Simplicity

\begin{itemize}
\item Reduced number of representation types
\item Invisible to user of |deriving|
\end{itemize}

\end{enumerate}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{|deriving Functor|}

\begin{spec}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
\end{spec}

PAUSE
\begin{itemize}INCREMENT
\item Need structure representation
\item Need type of parameter
\item Need location of elements with parameter type
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure Types}

We use the same structural elements we saw previously:

PAUSE
\begin{spec}
data      U1            p = U1

data      (f :*: g)     p = f p :*: g p

data      (f :+: g)     p = L1 { unL1 :: f p } | R1 { unR1 :: g p }

newtype   K1 i a        p = K1 { unK1 :: a }

newtype   M1 i c f      p = M1 { unM1 :: f p }
\end{spec}

PAUSE
Plus, we add a few more...

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure Types}

Parameter types:
\begin{code}
newtype Par1    p = Par1 { unPar1 :: p }
\end{code}

\begin{itemize}
\item Now, we see that |p| is used in the parameter position in |Par1|.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure Types}

Recursive types:
\begin{code}
newtype Rec1 f  p = Rec1 { unRec1 :: f p }
\end{code}

\begin{itemize}
\item Note that |Rec1| is actually more general than recursion, since |f| can be
any functorial type.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Type Representation}

The type representation has type-level and term-level components.

\begin{code}
class Generic1 f where
  type Rep1 f   :: * -> *
  from1         :: f p -> Rep1 f p
  to1           :: Rep1 f p -> f p
\end{code}

\begin{itemize}INCREMENT
\item Unlike with |Generic|, the |p| is used in the type.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Simplified Representation of Lists}

\begin{code}
instance Generic1 [] where
  type Rep1 [] = U1 :+: (RED(Par1)) :*: (RED(Rec1)) []

  from1 []        = L1  U1
  from1 (x : xs)  = R1  ((RED(Par1)) x :*: (RED(Rec1)) xs)

  to1 (L1  U1)                                  = []
  to1 (R1  ((RED(Par1)) x :*: (RED(Rec1)) xs))  = x : xs
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing the Generic |Functor|}
%if style /= newcode
%format Functor_ = Functor
%format fmap_ = fmap
%endif

\begin{code}
class Functor_ f where
  fmap_ :: (a -> b) -> f a -> f b
  default fmap_ :: (Generic1 f, Functor_ (Rep1 f)) => (a -> b) -> f a -> f b
  fmap_ = fmap_default
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing the Generic |Functor|}

\begin{code}
instance Functor_ U1 where
  fmap_ _ U1 = U1
\end{code}

PAUSE
\begin{code}
instance Functor_ (K1 i c) where
  fmap_ _ (K1 a) = K1 a
\end{code}

PAUSE
\begin{code}
instance (Functor_ f) => Functor_ (M1 i c f) where
  fmap_ f (M1 a) = M1 (fmap_ f a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing the Generic |Functor|}

\begin{code}
instance (Functor_ f, Functor_ g) => Functor_ (f :*: g) where
  fmap_ f (a :*: b) = fmap_ f a :*: fmap_ f b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing the Generic |Functor|}

\begin{code}
instance (Functor_ f, Functor_ g) => Functor_ (f :+: g) where
  fmap_ f (L1  a) = L1  (fmap_ f a)
  fmap_ f (R1  a) = R1  (fmap_ f a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing the Generic |Functor|}

\begin{code}
instance Functor_ Par1 where
  fmap_ f (Par1 a) = Par1 (f a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing the Generic |Functor|}

\begin{code}
instance (Functor_ f) => Functor_ (Rec1 f) where
  fmap_ f (Rec1 a) = Rec1 (fmap_ f a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing the Generic |Functor|}

\begin{code}
fmap_default :: (Generic1 f, Functor_ (Rep1 f)) => (a -> b) -> f a -> f b
fmap_default f = to1 . fmap_ f . from1
\end{code}

PAUSE
\begin{code}
instance Functor_ [] where
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Yet Another Generic Function}

Lest you think GD is not that useful, let's look at a very practical example:
the \verb=binary= package.

\begin{itemize}INCREMENT

\item ``Efficient, pure binary serialisation using lazy ByteStrings''

\item ``Serialisation speeds of over 1 G/sec have been observed, so this library
should be suitable for high performance scenarios.''

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Using \verb=binary=}

\begin{itemize}INCREMENT

\item Monads
\begin{itemize}
\item |Get| - a |State| monad carrying around the input |ByteString|
\item |Put| - a |Writer| monad over the efficient |Builder| monoid
\end{itemize}

\item Primitives
\begin{itemize}
\item |putWord8 :: Word8 -> Put|
\item |getWord8 :: Get Word8|
\end{itemize}

\item Serialization
\begin{itemize}
\item |encode :: (Binary a) => a -> ByteString|
\item |decode :: (Binary a) => ByteString -> a|
\end{itemize}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Using \verb=binary=}

\begin{spec}
class Binary t where
  put :: t -> Put

  get :: Get t
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic |Binary|}

\begin{code}
class Binary t where
  default put :: (Generic t, GBinary (Rep t)) => t -> Put

  put :: t -> Put
  put = put_default

  default get :: (Generic t, GBinary (Rep t)) => Get t

  get :: Get t
  get = get_default
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic |Binary|}

\begin{code}
class GBinary f where
  gput :: f a -> Put
  gget :: Get (f a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic |Binary|}

\begin{code}
instance GBinary U1 where
  gput U1  = return ()
  gget     = return U1
\end{code}

PAUSE
\begin{code}
instance (Binary a) => GBinary (K1 i a) where
  gput (K1 x)  = put x
  gget         = K1 <$> get
\end{code}

PAUSE
\begin{code}
instance (GBinary a) => GBinary (M1 i c a) where
  gput (M1 x)  = gput x
  gget         = M1 <$> gget
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic |Binary|}

\begin{code}
instance (GBinary f, GBinary g) => GBinary (f :*: g) where
  gput (x :*: y)  = gput x >> gput y
  gget            = (:*:) <$> gget <*> gget
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic |Binary|}

\begin{code}
instance (GBinary f, GBinary g) => GBinary (f :+: g) where
  gput (L1 x)  = putWord8 0 >> gput x
  gput (R1 y)  = putWord8 1 >> gput y

  gget = do  w <- getWord8
             case w of  0  -> L1  <$> gget
                        _  -> R1  <$> gget
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Writing a Generic |Binary|}

\begin{code}
put_default :: (Generic t, GBinary (Rep t)) => t -> Put
put_default = gput . from
\end{code}

\begin{code}
get_default :: (Generic t, GBinary (Rep t)) => Get t
get_default = to <$> gget
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{How Does GD Compare?}

\begin{itemize}

\item Compared to SYB:
\begin{itemize}
\item More efficient
\item Easier to implement some kinds of functions
\end{itemize}

\item Compared to other GP libraries:
\begin{itemize}
\item Being included with GHC makes things easier
\item Can't write some generic functions: folds, zips
\end{itemize}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Resources}

\begin{itemize}

\item Jos\'{e} Pedro Magalh\~{a}es, Atze Dijkstra, Johan Jeuring, and Andres
L\"{o}h. \href{http://www.dreixel.net/research/pdf/gdmh.pdf}{\color{blue} A
generic deriving mechanism for Haskell}. Proceedings of Haskell 2010. pp. 37–48.

\item
\href{http://www.haskell.org/ghc/docs/7.2.1/html/users_guide/generic-programming.html}{\color{blue}
Section 7.17 Generic Programming}. GHC User's Guide. Version 7.2.1.

\item This talk on GitHub:
\textcolor{blue}{\url{https://github.com/spl/dutchhug2011}}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------

\end{document}


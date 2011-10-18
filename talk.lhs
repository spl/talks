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
module Talk where
infixr 5 :+:
infixr 6 :*:
infixr 7 :.:
data List2 a = Nil2 | Cons2 a (List2 a)
deriving instance (Show a) => Show (List1 a)
deriving instance (Show a) => Show (List2 a)
deriving instance Functor List2
\end{code}
%endif

%-------------------------------------------------------------------------------
% Title

\title[Generic Deriving]{Generic Deriving}

\author{Sean Leather}

\institute{Utrecht University}

\date[2011-09-28]{28 September, 2011}

%-------------------------------------------------------------------------------

\begin{document}

%-------------------------------------------------------------------------------
\begin{frame}
\titlepage
\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{This time...}

Generic Deriving

\begin{itemize}

\item A DGP library (\verb=generic-deriving= on Hackage)

\item A few language extensions (in GHC 7.2)

\item Uses a type-indexed representation type 

\item Based on sum-of-products view

\item Adds support for parameterized types

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Why Generic Deriving?}

Problems:

\begin{itemize}
\item |deriving| mechanism in Haskell is currently built into the compiler
\item Its specification is informal is restricted to six classes: |Eq|, |Ord|,
|Enum|, |Bounded|, |Show|, and |Read|
\item GP libraries can be difficult to use: requiring instance creation,
Template Haskell, etc.
\end{itemize}

PAUSE
Goals of Generic Deriving:

\begin{itemize}
\item Increase flexibility and formalization of |deriving| mechanism
\item Improve ease of use of a GP library through compiler integration
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Design of Generic Deriving}

\begin{enumerate}INCREMENT

\item Support |deriving Functor| for types with a single parameter

\item Support |deriving| for |Eq|, |Ord|, |Enum|, |Bounded|, |Show|, and |Read|

\item Simplicity

\begin{itemize}
\item Reduced number of representation types
\item Invisible to user of |deriving|
\end{itemize}

\end{enumerate}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure Types}

These are the same structural elements we've seen before: unit, sum, and
product.

\begin{code}
data U1         p = U1

data (:+:) f g  p = L1 { unL1 :: f p } | R1 { unR1 :: g p }

data (:*:) f g  p = f p :*: g p
\end{code}

PAUSE
Plus, we add a few more: parameters and recursive tags.

\begin{code}
newtype Par1    p = Par1 { unPar1 :: p }

newtype Rec1 f  p = Rec1 { unRec1 :: f p }
\end{code}

These types make up the universe for this generic view.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Type Representation}

The type representation has type-level and term-level components.

\begin{code}
class Generic1 f where
  type Rep1 f   :: * -> *
  from1         :: f a -> Rep1 f a
  to1           :: Rep1 f a -> f a
\end{code}

\begin{itemize}INCREMENT

\item The associated type synonym |Rep1| is a \alert{type-indexed type}.

\item |f| is a normal Haskell type of kind |* -> *|.

\item |Rep1 f| is the type representation of |f| and also of kind |* -> *|.

\item |from1| and |to1| are the embedding-projection pair that witness the
isomorphism between the type and its representation.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Type Representation Example}

Recall |List1|:

\begin{code}
data List1 a = Nil1 | Cons1 a (List1 a)
\end{code}

PAUSE
The type representation of |List1| is defined by this instance of |Generic1|:

\begin{code}
instance Generic1 List1 where
  type Rep1 List1 = U1 :+: (RED(Par1)) :*: (RED(Rec1)) List1

  from1 Nil1          = L1  U1
  from1 (Cons1 x xs)  = R1  (Par1 x :*: Rec1 xs)

  to1 (L1  U1)                    = Nil1
  to1 (R1  (Par1 x :*: Rec1 xs))  = Cons1 x xs
\end{code}

Note the locations of |Par1| and |Rec1|.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Defining a Generic Function}

\begin{itemize}INCREMENT

\item A type class defines the type signature of a generic function.

\item A type class instance defines one \alert{type case} of a generic function.

\item The instance for the Haskell type includes the transformation |to1| or
|from1| its representation.

\item Since a primary design goal was to derive |Functor|, we will illustrate a
generic function by defining a generic |fmap|.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Defining a Generic |Functor|}

Recall the |Functor| class:

\begin{spec}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
\end{spec}

\begin{itemize}INCREMENT

\item This is our generic function signature.

\item We need instances for each representation type.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Functor|: Unit}

The unit type:

\begin{spec}
data U1 p = U1
\end{spec}

\begin{code}
instance Functor U1 where
  fmap _ U1 = U1
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Functor|: Sum}

The sum type:

\begin{spec}
data (:+:) f g  p = L1 { unL1 :: f p } | R1 { unR1 :: g p }
\end{spec}

\begin{code}
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (L1  a) = L1  (fmap f a)
  fmap f (R1  a) = R1  (fmap f a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Functor|: Product}

The product type:

\begin{spec}
data (:*:) f g p = f p :*: g p
\end{spec}

\begin{code}
instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (a :*: b) = fmap f a :*: fmap f b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Functor|: Parameter}

The parameter type:

\begin{spec}
newtype Par1 p = Par1 { unPar1 :: p }
\end{spec}

\begin{code}
instance Functor Par1 where
  fmap f (Par1 a) = Par1 (f a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Functor|: Recursion}

The recursion type:

\begin{spec}
newtype Rec1 f p = Rec1 { unRec1 :: f p }
\end{spec}

\begin{code}
instance (Functor f) => Functor (Rec1 f) where
  fmap f (Rec1 a) = Rec1 (fmap f a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Functor|: Final Words}

This function completes the generic function by allowing us to use it with any
type for which we can define a |Generic1| instance.

\begin{code}
fmapdefault :: (Generic1 f, Functor (Rep1 f)) => (a -> b) -> f a -> f b
fmapdefault f = to1 . fmap f . from1
\end{code}

PAUSE
To extend the universe for the generic |fmap| function, we give an instance of
the desired type.

\begin{code}
instance Functor List1 where
  fmap = fmapdefault
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Functor|: Default Signature}

In GHC 7.2, we can use the \verb=DefaultSignatures= language extension to define
a default implementation of |fmap|:

\begin{spec}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  default fmap :: (Generic1 f, Functor (Rep1 f)) => (a -> b) -> f a -> f b
  fmap = fmapdefault
\end{spec}

PAUSE
Then, we only need to give an empty instance:

\begin{spec}
instance Functor List1 where
\end{spec}

The default implementation of |fmap| is used along with its default type.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Other Representation Types}

There are a few other types used for structural representation.

PAUSE

|K1| is used for constant types (e.g. primitives) and other types for which we
do not have a representation type.

\begin{code}
newtype K1  i c    p = K1  { unK1 :: c }
\end{code}

PAUSE

|M1| is used for metadata of the datatype and data constructors.

\begin{code}
newtype M1  i c f  p = M1  { unM1 :: f p }
\end{code}

PAUSE

We distinguish the different sorts of metadata by proxy types.

\begin{columns}

\column{.28\textwidth}

\begin{code}
type D1  = M1 D
type C1  = M1 C
type S1  = M1 S
\end{code}

\column{.17\textwidth}

\begin{code}
data D
data C
data S
\end{code}

\column{.48\textwidth}

\begin{code}
-- Datatypes
-- Constructors
-- Selectors (record labels)
\end{code}

\end{columns}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Metadata Classes}

We access the metadata of different datatypes and constructors using these type
classes.

\begin{code}
class Datatype d where
  datatypeName  :: t d (f :: * -> *) p -> String
  moduleName    :: t d (f :: * -> *) p -> String
\end{code}

\begin{code}
class Constructor c where
  conName :: t c (f :: * -> *) p -> String
  emptyDots
\end{code}

PAUSE

Note how |datatypeName (M1 undefined)| uses the second type parameter of |M1| to
determine the datatype.

\begin{spec}
newtype M1 i c f p = M1 { unM1 :: f p }
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Metadata Example}

We distinguish the metadata of datatypes and constructors by proxy types.

\begin{code}
data SList
data SNil
data SCons
\end{code}

\begin{code}
instance Datatype SList where
  datatypeName _  = "List"
  moduleName _    = "Main"

instance Constructor SNil where
  conName _ = "Nil"
\end{code}

See \verb=Generics.Deriving.Show= for examples of how this is used.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Complete Type Representation Example}

The |List2| representation instance with metadata:

\begin{code}
instance Generic1 List2 where
  type Rep1 List2 = (RED(D1)) SList ((RED(C1)) SNil U1 :+: (RED(C1)) SCons (Par1 :*: Rec1 List2))

  from1 Nil2           = (RED(M1)) (L1  ((RED(M1)) U1))
  from1 (Cons2 x xs)   = (RED(M1)) (R1  ((RED(M1)) (Par1 x :*: Rec1 xs)))

  to1 ((RED(M1)) (L1  ((RED(M1)) U1)))                    = Nil2
  to1 ((RED(M1)) (R1  ((RED(M1)) (Par1 x :*: Rec1 xs))))  = Cons2 x xs
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Functor|: |K1| and |M1|}

We can also define cases of the generic |Functor| function for constants and
metadata.

\begin{code}
instance Functor (K1 i c) where
  fmap _ (K1 a) = K1 a

instance (Functor f) => Functor (M1 i c f) where
  fmap f (M1 a) = M1 (fmap f a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Supporting More Types}

\begin{question}
How do we define the representation for rose trees?
\end{question}

\begin{code}
data Rose a = Node a (List2 (Rose a))
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Type Composition}

We use an additional structure type to define the composition of two types.

\begin{code}
newtype (:.:) f g p = Comp1 { unComp1 :: f (g p) }
\end{code}

PAUSE

The |Rose| representation (minus the metadata) demonstrates this:

\begin{code}
instance Generic1 Rose where
  type Rep1 Rose = Par1 :*: List2 :.: Rec1 Rose

  from1 (Node x xs) = Par1 x :*: Comp1 ((RED(fmap)) (RED(Rec1)) xs)

  to1 (Par1 x :*: Comp1 xs) = Node x ((RED(fmap)) (RED(unRec1)) xs)
\end{code}

Note that the left argument type (|List2|) must be an instance of |Functor|.

PAUSE

We also can define the |Functor| case for |:.:|:

\begin{code}
instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp1 x) = Comp1 (fmap (fmap f) x)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic Functions for Kind |*|}

We have seen:

\begin{itemize}
\item How parameterized types are supported
\item A generic function for parameterized types
\end{itemize}

PAUSE

We will now look at:

\begin{itemize}
\item How we can support non-parameterized types
\item A generic function for non-parameterized types
\end{itemize}

PAUSE

... using the same structure types.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Another Type Representation}

Recall:

\begin{spec}
class Generic1 f where
  type Rep1 f   :: * -> *
  from1         :: f a -> Rep1 f a
  to1           :: Rep1 f a -> f a
\end{spec}

PAUSE

We use a different type-indexed representation type and embedding-projection
pair.

\begin{code}
class Generic a where
  type Rep a    :: * -> *
  from          :: a -> Rep a x
  to            :: Rep a x -> a
\end{code}

\begin{itemize}INCREMENT
\item |a| has kind |*|.
\item |Rep a| is parameterized.
\item |from| and |to| cannot know anything about the parameter.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Another Type Representation Example}

The |List2| representation:

\begin{code}
instance Generic (List2 a) where
  type Rep (List2 a) =
    D1 SList (C1 SNil U1 :+: C1 SCons ((RED(Par0)) a :*: (RED(Rec0)) (List2 a)))

  from Nil2          = M1 (L1  (M1 U1))
  from (Cons2 x xs)  = M1 (R1  (M1 (K1 x :*: K1 xs)))

  to (M1 (L1  (M1 U1)))                = Nil2
  to (M1 (R1  (M1 (K1 x :*: K1 xs))))  = Cons2 x xs
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{|Par0| and |Rec0|}

|Par0| and |Rec0| are not as important as |Par1| and |Rec1| for the
representation. Really, they are just tags for constant-like types:

\begin{columns}

\column{.48\textwidth}

\begin{code}
type Rec0  = K1 R
type Par0  = K1 P
\end{code}

\column{.48\textwidth}

\begin{code}
data R
data P
\end{code}

\end{columns}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Deriving |Generic|}

In GHC 7.2, we can use the \verb=DeriveGeneric= extension to encourage the
compiler to generate the |Generic| instance for us.

\begin{spec}
data List2 a = Nil2 | Cons2 a (List2 a) (RED(deriving)) (RED(Generic))
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Defining a Generic |Encode|}

Let's return to the generic encode function for an example. First, the
signature:

\begin{code}
data Bit = O | I

class Encode a where
  encode :: a -> [Bit]
\end{code}

PAUSE

|Encode| expects a type of kind |*|, but remember that our represenation is
actually still of kind |* -> *|.

PAUSE

So, we define a different generic function signature for representation types.

\begin{code}
class GEncode f where
  gencode :: f a -> [Bit]
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Encode|: Definition}

Defining the type cases is straightforward:

\begin{code}
instance GEncode U1 where
  gencode U1            = []

instance (GEncode a, GEncode b) => GEncode (a :+: b) where
  gencode (L1 x)        = O : gencode x
  gencode (R1 x)        = I : gencode x

instance (GEncode a, GEncode b) => GEncode (a :*: b) where
  gencode (x :*: y)     = gencode x ++ gencode y

instance (Encode a) => GEncode (K1 i a) where
  gencode (K1 x)        = encode x

instance (GEncode a) => GEncode (M1 i c a) where
  gencode (M1 x)        = gencode x
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic |Encode|: Final Words}

Our ``default'' function is also easy.

\begin{code}
gencodedefault :: (Generic a, GEncode (Rep a)) => a -> [Bit]
gencodedefault = gencode . from
\end{code}

Notice how the |GEncode| constraint is filled in with |Rep a|, which has kind |*
-> *|.

PAUSE

In GHC 7.2, we write our |Encode| class like this:

\begin{spec}
class Encode a where
  encode :: a -> [Bit]

  default encode :: (Generic a, GEncode (Rep a)) => a -> [Bit]
  encode = gencodedefault
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Resources}

\begin{itemize}

\item Manuel M. T. Chakravarty, Gabriel C. Ditu, and Roman Leshchinskiy. Instant
Generics: Fast and Easy.
\url{http://www.cse.unsw.edu.au/~chak/papers/CDL09.html}. 2009.

\item José Pedro Magalhães, Atze Dijkstra, Johan Jeuring, and Andres Löh. A
generic deriving mechanism for Haskell. In Proceedings of the third ACM Haskell
symposium on Haskell (Haskell'2010), pp. 37–48, ACM, 2010.

\item
\href{http://www.haskell.org/ghc/docs/latest/html/users_guide/generic-programming.html}{Section
7.17 Generic Programming}. GHC User's Guide. Version 7.2.1.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Next time...}

\begin{itemize}
\item Projects
\item Regular
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------

\end{document}


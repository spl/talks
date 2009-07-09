
\documentclass[fleqn]{beamer}

%-------------------------------------------------------------------------------
% Packages

\usepackage{talk}

%-------------------------------------------------------------------------------
% Formatting

% Global formatting directives
%include talk.fmt

%-------------------------------------------------------------------------------
% Commands

% Step the counter parameter by 1 and print the result
\newcommand{\showc}[1]{\stepcounter{#1}\arabic{#1}}

% Print the title with a count in parentheses
\newcommand{\CountingTitle}[2]{#1 (\showc{#2})}

\renewcommand{\b}[1]{\textbf{#1}}
\newcommand{\pkg}[1]{\texttt{#1}}

\newcommand{\bs}{%
%\setlength\abovedisplayskip{5pt}
\setlength\belowdisplayskip{-10pt}}

%-------------------------------------------------------------------------------
% Titles

\newcounter{countRepresentingStructure}
\newcommand{\RepresentingStructure}{\CountingTitle{Representing Structure in EMGM}{countRepresentingStructure}}

\newcounter{countDefiningEmpty}
\newcommand{\DefiningEmpty}{\CountingTitle{Defining Empty}{countDefiningEmpty}}

\newcounter{countDefiningCrush}
\newcommand{\DefiningCrush}{\CountingTitle{Defining Crush}{countDefiningCrush}}

\newcounter{countUsingCrush}
\newcommand{\UsingCrush}{\CountingTitle{Using Crush}{countUsingCrush}}

\newcounter{countAdhocInstances}
\newcommand{\AdhocInstances}{\CountingTitle{Ad Hoc Instances}{countAdhocInstances}}

\newcounter{countDefiningCollect}
\newcommand{\DefiningCollect}{\CountingTitle{Defining Collect}{countDefiningCollect}}

%-------------------------------------------------------------------------------

\begin{document}

%-------------------------------------------------------------------------------

%if style == newcode
\begin{code}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Talk where
import Prelude hiding (sum, any)

test ===  test1
     && test2
     && test3
     && test4
     && test5
     && test6
     && test7
     && test8
     && test9
     && test10
\end{code}
%endif

%-------------------------------------------------------------------------------

\title{\b{Fun} and \b{generic} things to do with \b{EMGM}}

\author{Sean Leather}

\date{9 July 2009}

%-------------------------------------------------------------------------------
\begin{frame}

\titlepage

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Extensible and Modular Generics for the Masses}

A powerful Haskell library that uses type classes with multiple parameters and
overlapping and undecidable instances to form a highly flexible foundation for
\b{datatype-generic programming} (\b{DGP}).

\onslide<2->
The \b{\pkg{emgm}} package on Hackage provides the following:

\begin{itemize}
\item Documented platform for writing generic functions
\item Flexible functionality for deriving instances using Template Haskell
\item Growing collection of useful generic functions
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{History of EMGM}

\begin{enumerate}

\item Published as \b{Generics for the Masses} by Ralf Hinze in 2004.

\item Revised by Bruno Oliveira, Andres Löh, and Hinze for extensibility and
modularity in 2006.

\item Explored further and compared with other DGP libraries by Alexey Rodriguez
Yakushev et al in 2007-2008.

\item Packaged and released by Sean Leather, Jos\'{e} Pedro Magalh\~{a}es, and
others at Utrecht University in September 2008.

\end{enumerate}

\onslide<2->
A tutorial is available as part of lecture notes created for the 2008 Advanced
Functional Programming Summer School.

\url{http://www.cs.uu.nl/research/techreps/UU-CS-2008-025.html}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Overview}

\begin{itemize}

\item Datatype-Generic Programming

\item Representing Datatypes in EMGM

\item Defining, Using, Extending Generic Functions

\begin{itemize}
\item Empty
\item Crush
\item Ad hoc Instances
\item Collect
\end{itemize}

\item Continuing Development of EMGM

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Datatype-Generic Programming}

\begin{itemize}

\item The term was coined by Jeremy Gibbons in ????, but the technique has been
around since at least ????.

\onslide<2->
\item Scrap Your Boilerplate (SYB) is an example of a popular DGP library.

\onslide<3->
\item DGP means generic on the \b{structure of a datatype}.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Structure of a Datatype}

\begin{itemize}

\item The structure is a way of representing the common aspects of many
datatypes, e.g. constructors, alternatives, tupling.

\onslide<2->
\item An intuitive way to determine the structure of a datatype is to look at
its declaration.

\end{itemize}

\bs
\begin{code}
data Tree aa === Tip | Leaf aa | Node Int (Tree aa) (Tree aa)
\end{code}
%if style == newcode
\begin{code}
deriving instance (Eq aa) => Eq (Tree aa)
deriving instance (Show aa) => Show (Tree aa)
\end{code}
%endif

\onslide<3->
\begin{itemize}
\item There are multiple \b{generic views} of the structure.
\item SYB uses one based on combinators.
\item EMGM uses a different one based on binary sums of products.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\RepresentingStructure}

\bs
\begin{spec}
data Tree aa === Tip | Leaf aa | Node Int (Tree aa) (Tree aa)
\end{spec}

To view the |Tree| type in its structure representation, we can substitute its
syntax with (nested) sums (alternatives) and products (pairs).

\bs
\begin{spec}
type TreeS' aa === UnitS + aa + Int * Tree aa * Tree aa
\end{spec}

\onslide<2->
Another way of looking at |TreeS'| using standard Haskell types is

\bs
\begin{code}
type TreeS' aa === Either UnitTuple (Either aa (Int, (Tree aa, Tree aa)))
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\RepresentingStructure}

While we might use standard Haskell types, we choose to use our own types for
readability and to prevent confusion between datatypes used in the
representation and those that are represented.

\onslide<2->
\bs
\begin{code}
data UnitT      === Unit          -- ()

data aa :*: bb  === aa ::*:: bb   -- (a, b)
infixr 6 :*:

data aa :+: bb  === L aa | R bb   -- Either a b
infixr 5 :+:

type TreeS aa === UnitT :+: aa :+: Int :*: Tree aa :*: Tree aa
\end{code}
%if style == newcode
\begin{code}
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
\end{code}
%endif

\onslide<3->
We also want to keep around meta-information about the constructors and the
types.

\bs
\begin{code}
data ConDescrT   === ConDescr dots

data TypeDescrT  === TypeDescr dots
\end{code}

%if style == newcode
\begin{code}
deriving instance Eq ConDescrT
deriving instance Ord ConDescrT
deriving instance Read ConDescrT
deriving instance Show ConDescrT
deriving instance Eq TypeDescrT
deriving instance Ord TypeDescrT
deriving instance Read TypeDescrT
deriving instance Show TypeDescrT
\end{code}
%endif

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\RepresentingStructure}

In order to access the structure of a datatype, we need to translate a value
from its native form to a representation form.

\onslide<2->
This is done using a \b{isomorphism} implemented as an \b{embedding-projection
pair}.

\bs
\begin{code}
data EPT dd rr === EP { from :: (dd -> rr), to :: (rr -> dd) }
\end{code}

\onslide<3->
Here is the |EPT| for |Tree|.

\begin{code}
epTree :: EPT (Tree aa) (TreeS aa)
epTree === EP fromTree toTree

  where  fromTree  Tip             === L Unit
         fromTree  (Leaf a)        === R (L a)
         fromTree  (Node i t1 t2)  === R (R (i ::*:: t1 ::*:: t2))

         toTree  (L Unit)                       === Tip
         toTree  (R (L a))                      === Leaf a
         toTree  (R (R (i ::*:: t1 ::*:: t2)))  === Node i t1 t2
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\RepresentingStructure}

A generic function is written by induction on the structure of a datatype. We
represent the cases of a function as methods of this (summarized) type class
|Generic|.

\onslide<2->
\bs
\begin{spec}
class Generic gg where
  rint       :: gg Int
  dots
  runit      :: gg UnitT
  rsum       :: gg aa -> gg bb -> gg (aa :+: bb)
  rprod      :: gg aa -> gg bb -> gg (aa :*: bb)
  rcon       :: ConDescrT -> gg aa -> gg aa
  rtype      :: TypeDescrT -> EPT bb aa -> gg aa -> gg bb
\end{spec}

\onslide<3->
Our \b{universe} supports constant types, structure types, and the ability to
extend the universe with arbitrary datatypes using |rtype|.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\RepresentingStructure}

To add a new datatype representation, we need to define an |rtype| value.

\bs
\begin{code}
rTree :: (Generic gg, Rep gg aa, Rep gg Int, Rep gg (Tree aa)) => gg (Tree aa)
rTree = rtype  (TypeDescr dots) epTree
               (  rcon (ConDescr dots)  runit  `rsum`
                  rcon (ConDescr dots)  rep    `rsum`
                  rcon (ConDescr dots)  (rep `rprod` rep `rprod` rep))
\end{code}

But what is this |Rep| and |rep| about?

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\RepresentingStructure}

To avoid having to provide all of these representations for every function, we
use another type class, |Rep|.

\bs
\begin{code}
class Rep gg aa where
  rep :: gg aa

instance  (Generic gg, Rep gg aa, Rep gg Int, Rep gg (Tree aa)) =>
          Rep gg (Tree aa) where
  rep = rTree
\end{code}

\onslide<2->
Of course, we also need instances for the constant types.

\bs
\begin{code}
instance (Generic gg) => Rep gg Int where
  rep === rint
dots
\end{code}
%if style == newcode
\begin{code}
instance (Generic gg) => Rep gg Char where
  rep === rchar
\end{code}
%endif

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generating the Structure Representation}

Fortunately, we don't have to write all of the previous boilerplate. We can
generate it using the Template Haskell functions included in the \pkg{emgm}
package.

\onslide<2->
\bs
\begin{spec}
$(derive ''Tree)
\end{spec}

This creates the |EPT|, the |ConDescrT|, the |TypeDescrT|, and all class
instances needed.

\onslide<3->
It is a good idea to understand what code is being derived. Use the following
pragma or command-line option in GHC to see the code generated at compile time:

\bs
\begin{spec}
{-#  OPTIONS -ddump-splices #-}
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{First Generic Function: \DefiningEmpty}

Now, we're ready to write our first generic function. Recall the |Generic| class
(in full).

\bs
\begin{code}
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
\end{code}
%if style == newcode
\begin{code}
  rint      === rconstant
  rinteger  === rconstant
  rfloat    === rconstant
  rdouble   === rconstant
  rchar     === rconstant
  runit     === rconstant
  rcon      === const id

infixr 6 `rprod`
infixr 5 `rsum`
\end{code}
%endif

\onslide<2->
A generic function is an instance of |Generic|. To write a function, we need to
produce a type for the instance.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningEmpty}

The simple function we're going to write is called |EmptyT|. It returns the
value of a type that is traditionally the initial value if you were to enumerate
all values. (|EnumT| is included in \pkg{emgm}.)

\onslide<2->
The type of the function is enclosed in a |newtype|.

\bs
\begin{code}
newtype EmptyT aa === Empty { selEmpty :: aa }
\end{code}

\onslide<2->
Note that the type of |selEmpty| gives a strong indication of the type of the
final function. For |EmptyT|, the type is identical (modulo class constraints),
but for some functions (such as |CrushT| that we see next), it can change.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningEmpty}

The function definition is straightforward.

\bs
\begin{code}
instance Generic EmptyT where
  rconstant                 === error "Should not be called!"
  rint                      === Empty 0
  rinteger                  === Empty 0
  rfloat                    === Empty 0
  rdouble                   === Empty 0
  rchar                     === Empty '\NUL'
  runit                     === Empty Unit
  rsum              ra  rb  === Empty (L (selEmpty ra))
  rprod             ra  rb  === Empty (selEmpty ra ::*:: selEmpty rb)
  rcon      cd      ra      === Empty (selEmpty ra)
  rtype     td  ep  ra      === Empty (to ep (selEmpty ra))
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningEmpty}

The ``core'' generic function is |selEmpty :: EmptyT aa -> aa|, but we wrap it
with a more usable function:

\bs
\begin{code}
empty :: (Rep EmptyT aa) => aa
empty === selEmpty rep
\end{code}

\onslide<2->
The primary purpose of |Rep| is to dispatch the appropriate type representation.

\onslide<3->
Applying |empty|:

\bs
\begin{code}
test1 === (empty :: Tree Int) == Tip
\end{code}

\onslide<4->
|EmptyT| has a very simple definition, and it may not be extremely useful, but
it demonstrates the basics of defining a generic function.

\onslide<5->
Let's move on to a more complicated function that is also much more useful.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{In Over Our Heads: \DefiningCrush}

\begin{itemize}

\item The generic function |CrushT| is sometimes called a generalization of the
list ``fold'' operations --- but so is a catamorphism.

\item It is also sometimes called ``reduce'' --- not exactly a precise
description.

\item To avoid confusion, let's not do any of these things and just focus on how
it works.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

\begin{itemize}

\item |CrushT| operates on the elements of a container or functor type.

\item It traverses all of the elements and accumulates a result that combines
them in some way.

\item In order to do this, |CrushT| requires a nullary value to initialize the
accumulator and a binary operation to combine an element with the accumulator.

\end{itemize}

\onslide<2->
We will define a function with a type signature similar to this:

\bs
\begin{spec}
crush :: (dots) => (aa -> bb -> bb) -> bb -> ff aa -> bb
\end{spec}

\onslide<3->
Notice the similarity:

\bs
\begin{spec}
foldr :: (aa -> bb -> bb) -> bb -> [aa] -> bb
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

\bs
\begin{spec}
crush :: (dots) => (aa -> bb -> bb) -> bb -> ff aa -> bb
\end{spec}

Our first challenge is to define the |newtype| for the function. Recall that
this gives a strong indication of the type of the function, but that it doesn't
necessarily match the final type exactly. Let's try to determine that type.

\onslide<2->
We have several major differences between the requirements for |EmptyT| and
those for |CrushT|.

\begin{enumerate}

\item |CrushT| takes arguments.

\item |CrushT| has three type variables over |EmptyT|'s one.

\item |CrushT| deals with a functor type (i.e. |ff :: # -> #|).

\end{enumerate}

\onslide<3->
Let's see how to deal with these.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

\bs
\begin{spec}
crush :: (dots) => (aa -> bb -> bb) -> bb -> ff aa -> bb
\end{spec}

1. \b{|CrushT| takes arguments.} This is not difficult to
handle. Our generic function cases can take arguments, too.

\onslide<2->
2. \b{|CrushT| has three type variables over |EmptyT|'s
one.} When defining a generic function in EMGM, it is important to determine
which types are actually ``generic'' (i.e. will need a structure representation)
and which types are not (e.g. may be polymorphic).

In this case, we traverse only the structure of the container, so the only truly
generic type variable is |ff|. Variables |aa| and |bb| are polymorphic.

\onslide<3->
2. \b{|CrushT| deals with a functor type (i.e. |ff :: # ->
#|).} Unfortunately, our current representation does not handle this.
Fortunately, the change is not large.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

We need a type class representation dispatcher for functor types.

\bs
\begin{code}
class FRep gg ff where
  frep :: gg aa -> gg (ff aa)
\end{code}

\onslide<2->
|FRep| allows us to represent the structure of a functor type while also giving
us access to the element type contained within.

\onslide<3->
Reusing our |Tree| example:

\bs
\begin{code}
instance (Generic gg) => FRep gg Tree where
  frep ra =
    rtype  (TypeDescr dots) epTree
           (  rcon (ConDescr dots)  runit  `rsum`
              rcon (ConDescr dots)  ra     `rsum`
              rcon (ConDescr dots)  (rint `rprod` frep ra `rprod` frep ra))
\end{code}

\onslide<4->
Again, this is generated code, and we don't have to write it.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

Now, back to defining the |newtype| for our function.

\bs
\begin{spec}
crush :: (dots) => (aa -> bb -> bb) -> bb -> ff aa -> bb
\end{spec}

\begin{itemize}

\onslide<2->
\item We need to determine the core functionality and choose the most general type.

\onslide<3->
\item The core generic functionality is to combine a value from a structure
representation with a non-generic value and return a non-generic result. This is
effectively the higher-order argument.

\end{itemize}

\onslide<4->
\bs
\begin{code}
newtype CrushT bb aa === Crush { selCrush :: aa -> bb -> bb }
\end{code}

\onslide<5->
We must be careful, however, to avoid thinking that this is the exact same as
the combining function. We are indicating two important aspects with this
declaration:

\begin{enumerate}
\item Type of the ``core'' generic function: type of |selCrush|
\item Which types are generic: type parameters of |CrushT|
\end{enumerate}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

Next, we define the function cases themselves.

\bs
\begin{code}
instance Generic (CrushT bb) where
\end{code}

\onslide<2->
The constant types (including |UnitT|) are simple. The constructor case is
almost as simple. The |rtype| case adds the conversion from the datatype.

\bs
\begin{code}
  rconstant         === Crush (const id)
  rcon   cd         === Crush . selCrush
  rtype  td  ep ra  === Crush (selCrush ra . from ep)
\end{code}

\onslide<3->
The sum case is somewhat more interesting.

\bs
\begin{code}
  rsum ra rb === Crush go
    where  go (L a)  === selCrush ra a
           go (R b)  === selCrush rb b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

The product case is even more interesting.

\bs
\begin{code}
  rprod ra rb === Crush go
    where  go (a ::*:: b) === selCrush ra a . selCrush rb b
\end{code}

\onslide<2->
Or should it be...?

\bs
\begin{spec}
  rprod ra rb === Crush go
    where  go (a ::*:: b) === selCrush rb b . selCrush ra a
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

Fortunately, we can turn this problem into a choice.

\bs
\begin{code}
data Assoc  ===  AssocLeft
            |  AssocRight

newtype CrushT2 bb aa === Crush2 { selCrush2 :: Assoc -> aa -> bb -> bb }
\end{code}

\onslide<2->
Then, we modify the product case (and others) with an associativity argument.

\bs
\begin{code}
instance Generic (CrushT2 bb) where
  dots
  rprod ra rb === Crush2 go
    where
      go s@AssocLeft   (a ::*:: b)  === selCrush2 rb s b . selCrush2 ra s a
      go s@AssocRight  (a ::*:: b)  === selCrush2 ra s a . selCrush2 rb s b
  dots
\end{code}
%if style == newcode
\begin{code}
  rconstant         === Crush2 (\_ _ -> id)
  rcon   cd         === Crush2 . selCrush2
  rtype  td  ep ra  === Crush2 (\s -> selCrush2 ra s . from ep)
  rsum ra rb === Crush2 go
    where  go s (L a)  === selCrush2 ra s a
           go s (R b)  === selCrush2 rb s b
\end{code}
%endif

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

We have defined the ``core'' generic function, so now we can define our
user-friendly wrapper.

\bs
\begin{spec}
crush :: (dots) => Assoc -> (aa -> bb -> bb) -> bb -> ff aa -> bb
\end{spec}

\onslide<2->
Let's first review the definitions we've collected.

\bs
\begin{spec}
Crush2     :: (Assoc -> aa -> bb -> bb) -> CrushT2 bb aa

frep       :: (FRep gg ff) => gg aa -> gg (ff aa)

selCrush2  :: CrushT2 bb aa -> Assoc -> aa -> bb -> bb
\end{spec}

\onslide<3->
Notice any patterns?

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

First, we need a higher-order combining function.

\bs
\begin{spec}
Crush2 :: (Assoc -> aa -> bb -> bb) -> CrushT2 bb aa
\end{spec}

\onslide<2->
Next, we need to transform the generic type parameter |aa| to a functional kind
using the new representation dispatcher.

\bs
\begin{spec}
frep :: (FRep gg ff) => gg aa -> gg (ff aa)

frep . Crush2 ::  (FRep (CrushT2 bb) ff) =>
                  (Assoc -> aa -> bb -> bb) -> CrushT2 bb (ff aa)
\end{spec}

\onslide<3->
Then, we open the |CrushT2| value to get the generic function.

\bs
\begin{spec}
selCrush2  :: CrushT2 bb aa -> Assoc -> aa -> bb -> bb

selCrush2 . frep . Crush2
  ::  (FRep (CrushT2 bb) ff) =>
      (Assoc -> aa -> bb -> bb) -> Assoc -> ff aa -> bb -> bb
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCrush}

Finally, with a little massaging, we can define |crush|.

\bs
\begin{code}
crush ::  (FRep (CrushT2 bb) ff) =>
          Assoc -> (aa -> bb -> bb) -> bb -> ff aa -> bb
crush s f z x === selCrush2 (frep (Crush2 (const f))) s x z
\end{code}

\onslide<2->
And we can define more wrappers that imply the associativity.

\bs
\begin{code}
crushl, crushr ::  (FRep (CrushT2 bb) ff) =>
                   (aa -> bb -> bb) -> bb -> ff aa -> bb
crushl === crush AssocLeft

crushr === crush AssocRight
\end{code}

\onslide<3->
That's it for the generic function definition, but what's the point? What can we
do with |CrushT2|?

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\UsingCrush}

Due to its genericity, |CrushT2| is a powerful and practical function. We can
build a large number of functions using |crush|.

\onslide<2->
\begin{itemize}
\item Flatten a container to a list of its elements:
\end{itemize}

\bs
\begin{code}
flattenr :: (FRep (CrushT2 [a]) f) => f a -> [a]
flattenr === crushr (:) []

test2 ===  flattenr (Node 2 (Leaf "Hi") (Leaf "London"))
         == ["Hi","London"]
\end{code}

\onslide<3->
\begin{itemize}
\item Or extract the reversed list:
\end{itemize}

\bs
\begin{code}
flattenl :: (FRep (CrushT2 [a]) f) => f a -> [a]
flattenl === crushl (:) []

test3 ===  flattenl (Node 2009 (Leaf 7) (Leaf 9)) == [9,7]
\end{code}

\onslide<4->
Notice the use of associativity.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\UsingCrush}

\begin{itemize}
\item Sum the (numerical) elements of a container:
\end{itemize}

\bs
\begin{code}
sum :: (Num a, FRep (CrushT2 a) f) => f a -> a
sum === crushr (+) 0

test4 === sum (Node 4 (Leaf 40) (Leaf 2)) == 42
\end{code}

\onslide<2->
\begin{itemize}
\item Or determine if any element satisfies a predicate:
\end{itemize}

\bs
\begin{code}
any :: (FRep (CrushT2 Bool) f) => (a -> Bool) -> f a -> Bool
any p === crushr (\x b -> b || p x) False

test5 === any (>2) (Node 5 (Leaf 0) (Leaf 1)) == False
\end{code}

\onslide<3->
The |CrushT2| function and its many derivatives are all available in the
\pkg{emgm} package.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Diversion: \AdhocInstances}

Let's deviate from defining generic functions for a bit and explore why EMGM is
extensible and modular. The reason is that we can override how a generic
function works for any datatype. The mechanism is called an \b{ad hoc instance}.

\onslide<2->
Suppose we want to change the ``empty'' value for |Tree Char|. The generic
result, as we have seen, is |Tip|, but we want something different.

\onslide<3->
\bs
\begin{code}
instance Rep EmptyT (Tree Char) where
  rep === Empty (Leaf empty)

test6 === empty == Leaf '\NUL'
\end{code}

The instance specifies the function signature, |EmptyT|, and the type for the
instance, |Tree Char|.

\onslide<4->
Note that you must have overlapping instances enabled for ad hoc instances:

\bs
\begin{spec}
{-# LANGUAGE OverlappingInstances #-}
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\AdhocInstances}

The example of |EmptyT| is simple to understand, but it does not do justice to
the flexibility that ad hoc instances provide.

Functions such as the |ReadT| and |ShowT| are very suitable for ad hoc
instances. Indeed, the \pkg{emgm} package uses them to support the special
syntax for lists and tuples.

\onslide<2->
\bs
\begin{spec}
instance (Rep ReadT aa) => Rep ReadT [aa] where
  rep === ReadT $ const $ list $ readPrec
\end{spec}

\onslide<3->
\bs
\begin{spec}
instance (Rep ShowT aa, Rep ShowT bb) => Rep ShowT (aa,bb) where
  rep === ShowT s
    where s _ _ (a,b) === showTuple [shows a, shows b]
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Return from Diversion: \DefiningCollect}

The last function we will define is also a useful one and takes full advantage
of ad hoc instances.

\onslide<2->
 The purpose of |CollectT| is to gather all (top-level) values of a certain type
from a value of a (different) type and return them in a list. |CollectT| relies
on a simple ad hoc instance for each type to match the collected value with the
result value.

\onslide<3->
The |newtype| is:

\bs
\begin{code}
newtype CollectT bb aa === Collect { selCollect :: aa -> [bb] }
\end{code}

\onslide<4->
The type parameter |aa| represents the generic collected type, and |bb|
represents the non-generic result type.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCollect}

Now, onto the definition.

\onslide<2->
As with |CrushT|, the constant types (including |UnitT|) and the cases for
constructors and types are quite straightforward.

\bs
\begin{code}
instance Generic (CollectT b) where
  rconstant            === Collect (const [])
  rcon   cd      ra    === Collect (selCollect ra)
  rtype  td  ep  ra    === Collect (selCollect ra . from ep)
\end{code}

\onslide<3->
The key to keep in mind with this generic function is that the structural
induction simply recurses throughout the value. It is the ad hoc instances that
do the important work.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCollect}

The sum case recursively dives into the indicated alternative.

\bs
\begin{code}
  rsum ra rb === Collect go
    where  go (L a)  === selCollect ra a
           go (R b)  === selCollect rb b
\end{code}

\onslide<2->
The product case appends the collected results of one component to those of the
other.

\bs
\begin{code}
  rprod ra rb === Collect go
    where  go (a ::*:: b) === selCollect ra a ++ selCollect rb b
\end{code}

\onslide<3->
The wrapper itself is quite simple.

\bs
\begin{code}
collect :: (Rep (CollectT bb) aa) => aa -> [bb]
collect === selCollect rep
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\DefiningCollect}

So, what about the ad hoc instances? Here is an example:

\bs
\begin{code}
instance Rep (CollectT Int) Int where
  rep === Collect (:[])
\end{code}
%if style == newcode
\begin{code}
instance Rep (CollectT Char) Char where
  rep === Collect (:[])
\end{code}
%endif

\onslide<2->
And here's another:

\bs
\begin{code}
instance Rep (CollectT (Tree aa)) (Tree aa) where
  rep === Collect (:[])
\end{code}

\onslide<3->
(And guess what? This is generated by |$(derive ''Tree)|!)

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Using Collect}

The function |collect| is easy to use...

\bs
\begin{code}
val1 === Node 88 (Leaf 'a') (Leaf 'b')

test7 ===  collect val1 == "ab"

test8 === collect val1 == [88 :: Int]
\end{code}

\onslide<2->
... as long as you remember that the result type must be non-polymorphic and
non-ambiguous.

\onslide<3->
\bs
\begin{code}
val2 :: Tree Int
val2 === (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))

test9 ===  collect val2 == [1,2,3,4,5 :: Int]

test10 ===  collect val2 == [val2]
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Looking at \pkg{emgm}}

We have discussed how to write several generic functions. If you set off to
implement your own, you should have a good idea of where to start.

\onslide<2->
If you instead want to simply use the available generic functions in the
\pkg{emgm} package, you should look at the Haddock docs:

\url{http://hackage.haskell.org/package/emgm/}

\onslide<3->
(Yes, look at it now...)

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Continuing Development of EMGM}

EMGM is continuing to evolve. We have plans for a number of new functions or
packages.

\begin{itemize}
\onslide<2->
\item |transpose :: f (g a) -> g (f a)|
\onslide<3->
\item Map with first-class higher-order generic function
\onslide<4->
\item Supporting \pkg{binary}, \pkg{bytestring}, \pkg{HDBC}
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Cheers!}

We would also be happy to take bug reports, contributions, or see other packages
using \pkg{emgm}.

All roads to more information start at the homepage.

\url{http://www.cs.uu.nl/wiki/GenericProgramming/EMGM}

\end{frame}
%-------------------------------------------------------------------------------
\end{document}


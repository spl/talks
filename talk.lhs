
\documentclass[fleqn]{beamer}

%-------------------------------------------------------------------------------
% Packages

\usepackage{talk}

%-------------------------------------------------------------------------------
% Commands

% Step the counter parameter by 1 and print the result
\newcommand{\showc}[1]{\stepcounter{#1}\arabic{#1}}

% Print the title with a count in parentheses
\newcommand{\CountingTitle}[2]{#1 (\showc{#2})}

\renewcommand{\b}[1]{\textbf{#1}}
\newcommand{\pkg}[1]{\texttt{#1}}

%-------------------------------------------------------------------------------
% Titles

\newcounter{countRepresentingStructure}
\newcommand{\RepresentingStructure}{\CountingTitle{Representing Structure in EMGM}{countRepresentingStructure}}

\newcounter{countGenericFunctionEmpty}
\newcommand{\GenericFunctionEmpty}{\CountingTitle{First Generic Function: Empty}{countGenericFunctionEmpty}}

%-------------------------------------------------------------------------------
% Formatting

% Global formatting directives
%include talk.fmt

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
module Talk where

test = test1
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

\item Published as \b{Generics for the Masses} by Ralf Hinze in ????

\item Revised by Bruno ???? Oliveira, Andres Löh, and Hinze for extensibility
and modularity in 2006.

\item Explored further and compared with other DGP libraries by Alexey Rodriguez
Yakushev et al in 2007-2008.

\item Packaged and released by Sean Leather, Jos\'{e} Pedro Magalh\~{a}es, and
others at Utrecht University in September 2008.

\end{enumerate}

A tutorial is available as part of lecture notes created for the 2008 Advanced
Functional Programming Summer School.

http://...

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{Overview}

\begin{itemize}

\item Datatype-Generic Programming

\item Representing Datatypes in EMGM

\begin{itemize}
\item Structural Representation
\item Deriving with Template Haskell
\end{itemize}

\item Defining Generic Functions

\begin{itemize}
\item Empty, Crush, Collect
\end{itemize}

\item Using Generic Functions

\begin{itemize}
\item Ad-hoc Instances
\item Map, ZipWith
\end{itemize}

\item Continuing Development of EMGM

\begin{itemize}
\item Future Generic Functions (transpose)
\item New Packages (binary, bytestring, database, more?)
\end{itemize}

\end{itemize}

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{Datatype-Generic Programming}

\begin{itemize}

\item The term was coined by Jeremy Gibbons in ????, but the technique has been
around since at least ????.

\item Scrap Your Boilerplate (SYB) is an example of a popular DGP library.

\item DGP means generic on the \b{structure of a datatype}.

\end{itemize}

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{Structure of a Datatype}

The structure is a way of representing the common aspects of many datatypes,
e.g. constructors, alternatives, tupling. An intuitive way to determine the
structure of a datatype is to look at its declaration.

\setlength\belowdisplayskip{0pt}
\begin{code}
data Tree aa = Tip | Leaf aa | Node Int (Tree aa) (Tree aa)
\end{code}

%if style == newcode
\begin{code}
deriving instance (Eq aa) => Eq (Tree aa)
deriving instance (Show aa) => Show (Tree aa)
\end{code}
%endif

There are multiple \b{generic views} of the structure. SYB uses one based on
combinators. EMGM uses a different one based on binary sums of products.

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\RepresentingStructure}

To view the |Tree| type,

\setlength\belowdisplayskip{0pt}
\begin{spec}
data Tree aa = Tip | Leaf aa | Node Int (Tree aa) (Tree aa)
\end{spec}

in its structure representation, we can substitute its syntax with (nested) sums
(alternatives) and products (pairs).

\setlength\belowdisplayskip{0pt}
\begin{spec}
type TreeS' aa = UnitS + aa + Int * Tree aa * Tree aa
\end{spec}

Another way of looking at |TreeS'| using standard Haskell types is

\setlength\belowdisplayskip{0pt}
\begin{code}
type TreeS' aa = Either UnitTuple (Either aa (Int, (Tree aa, Tree aa)))
\end{code}

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\RepresentingStructure}

While we might use standard Haskell types, we choose to use our own types for
readability and to prevent confusion between datatypes used in the
representation and those that are represented.

\setlength\belowdisplayskip{0pt}
\begin{code}
data UnitT      = Unit          -- ()

data aa :*: bb  = aa ::*:: bb   -- (a, b)
infixr 6 :*:

data aa :+: bb  = L aa | R bb   -- Either a b
infixr 5 :+:

type TreeS aa = UnitT :+: aa :+: Int :*: Tree aa :*: Tree aa
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

We also want to keep around meta-information about the constructors and the
types.

\setlength\belowdisplayskip{0pt}
\begin{code}
data ConDescrT   = ConDescr dots

data TypeDescrT  = TypeDescr dots
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
from its native form to a representation form. This is done using a
\b{isomorphism} implemented as an \b{embedding-projection pair}.

\setlength\belowdisplayskip{0pt}
\begin{code}
data EPT dd rr = EP { from :: (dd -> rr), to :: (rr -> dd) }
>-<
epTree :: EPT (Tree aa) (TreeS aa)
epTree = EP fromTree toTree

  where  fromTree  Tip             = L Unit
         fromTree  (Leaf a)        = R (L a)
         fromTree  (Node i t1 t2)  = R (R (i ::*:: t1 ::*:: t2))

         toTree  (L Unit)                       = Tip
         toTree  (R (L a))                      = Leaf a
         toTree  (R (R (i ::*:: t1 ::*:: t2)))  = Node i t1 t2
\end{code}

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\RepresentingStructure}

A generic function is written by induction on the structure of a datatype. We
represent the cases of a function as methods of this (summarized) type class
|Generic|.

\setlength\belowdisplayskip{0pt}
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

Our \b{universe} supports constant types, structure types, and the ability to
extend the universe with arbitrary datatypes using |rtype|.

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\RepresentingStructure}

To add a new datatype representation, we need to provide an |rtype| value. To
avoid having to provide all of these representations for every function, we use
another type class, |Rep|.

\setlength\belowdisplayskip{0pt}
\begin{code}
class Rep gg aa where
  rep :: gg aa

instance (Generic gg, Rep gg aa) => Rep gg (Tree aa) where
  rep =  rtype  (TypeDescr dots)
                epTree
                (  rcon (ConDescr dots)  runit  `rsum`
                   rcon (ConDescr dots)  rep    `rsum`
                   rcon (ConDescr dots)  (rep `rprod` rep `rprod` rep))
\end{code}

Of course, we also need instances for the constant types.

\setlength\belowdisplayskip{0pt}
\begin{code}
instance (Generic gg) => Rep gg Int where rep = rint
dots
\end{code}

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{Generating the Structure Representation}

Fortunately, we don't have to write all of the previous boilerplate. We can
generate it using the Template Haskell functions included in the \pkg{emgm}
package.

\setlength\belowdisplayskip{0pt}
\begin{spec}
$(derive ''Tree)
\end{spec}

This creates the |EPT|, the |ConDescrT|, the |TypeDescrT|, and all class
instances needed.

It is a good idea to understand what code is being derived. Use the following
pragma or command-line option in GHC to see the code generated at compile time:

\setlength\belowdisplayskip{0pt}
\begin{spec}
{-#  OPTIONS -ddump-splices #-}
\end{spec}

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\GenericFunctionEmpty}

Now, we're ready to write our first generic function. Recall the |Generic| class
(in full).

\setlength\belowdisplayskip{0pt}
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
  rint      = rconstant
  rinteger  = rconstant
  rfloat    = rconstant
  rdouble   = rconstant
  rchar     = rconstant
  runit     = rconstant
  rcon      = const id

infixr 6 `rprod`
infixr 5 `rsum`
\end{code}
%endif

A generic function is an instance of |Generic|. To write a function, we need to
produce a type for the instance.

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\GenericFunctionEmpty}

The function we're going to write is called |EmptyT|. It returns the value of a
type that is traditionally the initial value if you were to enumerate all
values. (|EnumT| is included in \pkg{emgm}.)

The type of the function is enclosed in a |newtype|.

\setlength\belowdisplayskip{0pt}
\begin{code}
newtype EmptyT aa = Empty { selEmpty :: aa }
\end{code}

Note that the type of |selEmpty| gives a strong indication of the type of the
final function. For |EmptyT|, the type is identical (modulo class constraints),
but for some functions (such as |CrushT| that we see next), it can change.

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\GenericFunctionEmpty}

The function definition is straightforward.

\setlength\belowdisplayskip{0pt}
\begin{code}
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
\end{code}

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\GenericFunctionEmpty}

The ``core'' generic function is |selEmpty :: EmptyT aa -> aa|, but we wrap it
with a more usable function:

\setlength\belowdisplayskip{0pt}
\begin{code}
empty :: (Rep EmptyT aa) => aa
empty = selEmpty rep
\end{code}

Applying |empty|:

\setlength\belowdisplayskip{0pt}
\begin{code}
test1 = (empty :: Tree Int) == Tip
\end{code}

|EmptyT| has a very simple definition, and it may not be extremely useful, but
it demonstrates the basics of defining a generic function.

Let's move on to a more complicated function that is also much more useful.

\end{frame}

%-------------------------------------------------------------------------------

\end{document}


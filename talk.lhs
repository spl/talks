
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

%-------------------------------------------------------------------------------
% Titles

\newcounter{countRepresentingStructure}
\newcommand{\RepresentingStructure}{\CountingTitle{Representing Structure in EMGM}{countRepresentingStructure}}

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

The \b{emgm} package on Hackage provides the following:

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

\begin{code}
data Tree aa = Tip | Leaf aa | Node Int (Tree aa) (Tree aa)
\end{code}

There are multiple \b{generic views} of the structure. SYB uses one based on
combinators. EMGM uses a different one based on binary sums of products.

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\RepresentingStructure}

To view the |Tree| type,

\begin{spec}
data Tree aa = Tip | Leaf aa | Node Int (Tree aa) (Tree aa)
\end{spec}

in its structure representation, we can substitute its syntax with (nested) sums
(alternatives) and products (pairs).

\begin{spec}
type TreeS' aa = UnitS + aa + Int * Tree aa * Tree aa
\end{spec}

Another way of looking at |TreeS'| using standard Haskell types is

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

\begin{code}
data EPT dd rr = EP { from :: (dd -> rr), to :: (rr -> dd) }

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
represent these cases as methods of a type class.

\begin{code}
infixr 6 `rprod`
infixr 5 `rsum`

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
\end{code}
%endif

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{\RepresentingStructure}

To add a new datatype representation, we need to provide an |rtype| value. To
avoid having to provide all of these representations for every function, we use
another type class, |Rep|.

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

Of course, we need instances for the constant types.

\begin{code}
instance (Generic gg) => Rep gg Int where rep = rint

dots
\end{code}

\end{frame}

%-------------------------------------------------------------------------------

\end{document}


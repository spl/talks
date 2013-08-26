\documentclass{beamer}

%-------------------------------------------------------------------------------
% C Preprocessor Directives

#define DO_PAUSES

#include "pause.h"

%-------------------------------------------------------------------------------
% Packages

\usepackage{listings} % for code listings
\lstset{
  columns=fullflexible,
  aboveskip=-1pt,
  belowskip=1pt,
  showstringspaces=false,
}

%-------------------------------------------------------------------------------
% Formatting (includes)

%include talk.fmt
%include forall.fmt

%-------------------------------------------------------------------------------
% Formatting (specific)

%-------------------------------------------------------------------------------
% Commands

\newcommand{\CPP}{C\nolinebreak\hspace{-.05em}\raisebox{.4ex}{\tiny\bf +}\nolinebreak\hspace{-.10em}\raisebox{.4ex}{\tiny\bf +}}
\newcommand{\CSharp}{C\nolinebreak\hspace{-.05em}\raisebox{.6ex}{\tiny\bf \#}}

%-------------------------------------------------------------------------------
% Titles

%-------------------------------------------------------------------------------
% Code

%if style == newcode
\begin{code}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Talk where
import qualified Text.Printf as TP
\end{code}
%endif

%if style /= newcode
%format TP.printf = printf
%endif

%if style /= newcode
%format Alt_1
%format Alt_2
%format Rep_D
%format Rep_D' = Rep_D
%endif

\newcommand{\dataD}{%
\begin{code}
data D (PURPLE(p)) = (GREEN(Alt_1)) | (GREEN(Alt_2)) (BLUE(Int)) (BLUE(p))
\end{code}
}

%if style /= newcode
%format show_unit = "\Varid{show_{U}}"
%format show_con = "\Varid{show_{C}}"
%format show_prod = "\Varid{show_{\times}}"
%format show_sum = "\Varid{show_{+}}"
%format show_int = "\Varid{show_{Int}}"
%format show_a = "\Varid{show_{a}}"
%format show_b = "\Varid{show_{b}}"
%format show_Rep_D = "\Varid{show_{" Rep_D' "}}"
%format show_Rep_D' = "\Varid{show_{" Rep_D' "}^{\prime}}"
%format show_D = "\Varid{show_{" D "}}"
%format show_D' = "\Varid{show_{" D "}^{\prime}}"
%format show_p = "\Varid{show_{" p "}}"
%endif

%if style == newcode
\begin{code}
show_int :: Int -> String
show_int = show
\end{code}
%endif

\newcommand{\showD}{%
\begin{code}
show_Rep_D :: (p -> String) -> Rep_D' p -> String
show_Rep_D show_p =
  show_sum (show_con show_unit) (show_con (show_prod show_int show_p))
\end{code}
}

%-------------------------------------------------------------------------------
% Title

\title[Dissecting Flavors of GP in Haskell]{Dissecting Different Flavors of Generic Programming in One Haskell Universe}
\subtitle{Presented to Galois}

\author{Sean Leather}

\institute{Utrecht University}

\date[2013-08-27]{August 27, 2013}

%-------------------------------------------------------------------------------

\begin{document}

%-------------------------------------------------------------------------------
\begin{frame}

\titlepage

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{What is \alert{Generic Programming}?}

PAUSE

In programming languages, the adjective ``generic'' is heavily overloaded.

PAUSE_LINE

\begin{itemize}INCREMENT
\item Java/\CSharp{} generics
\item \CPP{} templates
\item Ada generic packages
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{What is Generic Programming?}

\begin{block}{The goal is often the same.}
A higher level of abstraction than ``normally'' available
\end{block}

PAUSE_LINE

\begin{block}{The technique is also often similar.}
Some form of parameterization and instantiation
\end{block}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}[fragile]
\frametitle{Examples of Generic Programming}

Java/\CSharp{}:
\begin{beamerboxesrounded}{}
\begin{lstlisting}[language=Java]
public class Stack<T>
{
  public void push(T item) {...}
  public T pop() {...}
}
\end{lstlisting}
\end{beamerboxesrounded}

PAUSE_LINE

In other words:
\begin{itemize}
\item Java-style generics \(\approx\) parametric polymorphism
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}[fragile]
\frametitle{Examples of Generic Programming}

\CPP{}:
\begin{beamerboxesrounded}{}
\begin{lstlisting}[language=C++]
template<typename T, typename Compare>
T& min(T& a, T& b, Compare comp) {
  if (comp(b, a))
    return b;
  return a;
}
\end{lstlisting}
\end{beamerboxesrounded}

PAUSE_LINE

In other words:
\begin{itemize}
\item \CPP{} templates \(\approx\) ad-hoc polymorphism
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Programming in Haskell}

``Generic programming'':
\begin{itemize}INCREMENT
\item For other languages, the term tends to be used for late additions.
\item Parametric and ad-hoc polymorphism were available in Haskell from the beginning.
\end{itemize}

PAUSE_LINE

In Haskell, we have come to use ``generic programming'' for
\alert{datatype-generic programming} (a.k.a.\ ``polytypism'' or
``shape/structure polymorphism'').

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Datatype-Generic Programming}

What is datatype-generic programming?
\begin{itemize}INCREMENT
\item Parameterize a function over the \emph{structure} of datatypes
\item Instantiate the function with a particular type
\end{itemize}

PAUSE_LINE

The result is a function that
\begin{itemize}INCREMENT
\item \textbf{works with many types} (polymorphism) but
\item \textbf{uses knowledge of the type} (unlike parametric) and
\item \textbf{need not be redefined for every type} (unlike ad-hoc).
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Functions}

Applications
\begin{itemize}INCREMENT
\item Pretty-printing (e.g.\ |show|), parsing (e.g.\ |read|)
\item Compression, serialization, marshalling (and their inverses)
\item Comparison, equality
\item (Co-)recursion, map, zip, zippers
\item Traversals, queries, updates
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Platforms}

Many different implementations:
\begin{itemize}INCREMENT

\item Preprocessors:

\begin{itemize}INCREMENT
\item PolyP
\item Generic Haskell
\end{itemize}

\item Libraries

\begin{itemize}INCREMENT
\item Scrap Your Boilerplate (SYB) -- included with GHC for a long time
\item Extensible and Modular Generics for the Masses (EMGM)
\item Regular -- recursion schemes
\item Multirec -- mutually recursive datatypes
\item Generic Deriving -- available in GHC \(\geq\) 7.2, similar to Instant Generics
\item (and many, many more)
\end{itemize}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Flavors}

The implementations can be grouped into flavors depending on how they view the
structure of a datatype.

PAUSE_LINE

Some flavors (or views):
\begin{description}INCREMENT[Sums-of-products]

\item[Spine] A constructor is a sequence of types. \\ Example: SYB

\item[Sums-of-products] A datatype is a collection of alternative tuples of
types.
\\ Example: Generic Deriving

\item[Fixed point] A datatype is a sums-of-products with recursive points.
\\ Example: Multirec

\end{description}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Dissecting a Datatype: Sums-of-Products}

%if style /= newcode
%format T_sum
%format A_1
%format A_2
%endif

\begin{code}
data T_sum = (GREEN(A_1)) | (GREEN(A_2))
\end{code}

LINE

A datatype can have:
\begin{itemize}
\item \GREEN{Alternatives}: unique constructors (\(\geq 0\))
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Dissecting a Datatype: Sums-of-Products}

%if style /= newcode
%format T_prod
%format P_2
%endif

\begin{code}
data T_prod = (GREEN(P_2)) (BLUE(Char)) (BLUE(Int))
\end{code}

LINE

A datatype can have:
\begin{itemize}
\item \BLUE{Fields}: types for each constructor (\(\geq 0\))
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Dissecting a Datatype: Sums-of-Products}

Other features that are modeled:
\begin{itemize}
\item Constant types: each type in a field
\item Parameters: type variables (\(\geq 0\))
\end{itemize}

PAUSE_LINE

Features that are not modeled:
\begin{itemize}
\item Recursion
\item Nesting (though it can be)
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Modeling a Sum}

To model (nested) alternatives:
\begin{spec}
data Either a b = Left a | Right b
\end{spec}

PAUSE_LINE

For syntactic elegance:
\begin{code}
data a :+: b = L a | R b
\end{code}
%if style == newcode
\begin{code}
infixr 5 :+:
\end{code}
%endif

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Modeling a Product}

To model (nested) fields:
\begin{spec}
data (,) a b = (,) a b
\end{spec}

PAUSE_LINE

For syntactic elegance:
\begin{code}
data a :*: b = a :*: b
\end{code}
%if style == newcode
\begin{code}
infixr 6 :*:
\end{code}
%endif

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Modeling Other Structures}

A constructor without fields:
\begin{code}
data U = U
\end{code}

PAUSE_LINE

A constructor name:
\begin{code}
data C a = C String a
\end{code}

PAUSE_LINE

A field type:
\begin{code}
data K a = K a
\end{code}

PAUSE_LINE

There are other features of datatypes, but we will consider only the above as a
foundation for looking at the structure.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Modeling an Example}

%if style /= newcode
%format E_1
%format E_2
%format Rep_E
%endif

An example datatype:
\begin{code}
data E a = (GREEN(E_1)) | (GREEN(E_2)) (BLUE(a)) (BLUE((E a))) (BLUE(Int))
\end{code}

PAUSE_LINE

The corresponding \alert{structure representation type}:
\begin{code}
type Rep_E a = (GREEN(C)) U :+: (GREEN(C)) ((BLUE(K)) a :*: (BLUE(K)) (E a) :*: (BLUE(K)) Int)
\end{code}

PAUSE_LINE

Notes:
\begin{itemize}
\item |:+:| is |infixr 5| and |:*:| is |infixr 6|.
\item Operators nest to the right.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Converting Between Types: Isomorphism}

%if style /= newcode
%format from_E
%format to_E
%endif

\begin{itemize}INCREMENT

\item Generic functions work on the sums-of-products model.

\item But first we need to convert between the model and the actual value of the
datatype.

\item We define an \alert{isomorphism}: two total, dual functions.

\end{itemize}

PAUSE_LINE

\begin{code}
from_E :: E a -> Rep_E a
from_E  E_1          = L (C "E1" U)
from_E  (E_2 x e i)  = R (C "E2" ((K x) :*: (K e) :*: (K i)))
\end{code}

PAUSE_LINE

\begin{code}
to_E :: Rep_E a -> E a
to_E  (L (C "E1" U))                            = E_1
to_E  (R (C "E2" ((K x) :*: (K e) :*: (K i))))  = E_2 x e i
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Structure of Datatypes: Constructors}

Oh, but there's one more thing...

PAUSE

You may have noticed the representation lacked any information about the
constructors (e.g.\ the names).

PAUSE_LINE

That's easily repaired with another datatype:

PAUSE_LINE

We modify the representation to store constructor names:
\begin{code}
type Rep_D' (PURPLE(p)) = C (BLUE(U)) :+: C ((BLUE(Int)) :*: (BLUE(p)))

from_D'  Alt_1        = L (C "Alt1" U)
from_D'  (Alt_2 i p)  = R (C "Alt2" (i :*: p))
\end{code}
%if style == newcode
\begin{code}
to_D'  (L (C nm U))          = Alt_1
to_D'  (R (C nm (i :*: p)))  = Alt_2 i p
\end{code}
%endif

PAUSE

We could also put additional metadata (e.g.\ fixity) into |C|.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Functions}

Okay, so we have a structure representation. But what can we \emph{do} with it?

PAUSE_LINE

\alert{Generic functions}
\begin{itemize}INCREMENT

\item Defined on each possible case of the structure representation

\item Work for every datatype that has an isomorphism with a structure
representation

\end{itemize}

PAUSE_LINE

Example: |show :: a -> String|

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Functions: |show|}

We define a |show| function for each possible structure case.

PAUSE_LINE

\begin{columns}
\column{.38\textwidth}
Unit:
\begin{code}
show_unit :: U -> String
show_unit U = ""
\end{code}
\column{.58\textwidth}
PAUSE
Constructor name:
\begin{code}
show_con :: (a -> String) -> C a -> String
show_con show_a (C nm a) =
  "(" ++ nm ++ " " ++ show_a a ++ ")"
\end{code}
\end{columns}

PAUSE_LINE

Binary product:
\begin{code}
show_prod :: (a -> String) -> (b -> String) -> a :*: b -> String
show_prod show_a show_b (a :*: b) = show_a a ++ " " ++ show_b b
\end{code}

PAUSE_LINE

Binary sum:
\begin{code}
show_sum :: (a -> String) -> (b -> String) -> a :+: b -> String
show_sum show_a _ (L a) = show_a a
show_sum _ show_b (R b) = show_b b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Functions: |show|}

We can define a |show| function for |Rep_D'| (assuming |show_int|):
\showD

PAUSE_LINE

The |show| function for |D| is just a hop away:
\begin{code}
show_D :: (p -> String) -> D p -> String
show_D show_p = show_Rep_D show_p . from_D'
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Functions: |show|}

\showD

LINE

Some observations:
\begin{itemize}INCREMENT

\item This is a sort of predictable pattern (or recipe) for defining |show|
functions on structure representations.

\item The functions are recursive but not in the usual way because the argument
types differ.

\item Each datatype can have a unique structure representation, and we want to
support all combinations, \emph{generically}.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Functions, Generically}

%if style /= newcode
%format Rep_T
%endif

In order to jump into ``true'' genericity (where the structure is a parameter
instead of a pattern), we need several addtional things:

PAUSE

\begin{itemize}INCREMENT

\item \alert{Polymorphic recursion} -- functions with a common scheme that
reference each other and allow types to change in the calls
PAUSE
\begin{spec}
show_unit  ::         U         -> String
show_con   :: ... =>  C a       -> String
show_sum   :: ... =>  a :+: b   -> String
...
\end{spec}

\item A common encoding for isomorphisms
PAUSE
\begin{spec}
data  T      = ...  -- User-defined datatype
type  Rep_T  = ...  -- Structure representation
from  :: T -> Rep_T
to    :: Rep_T -> T
\end{spec}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Polymorphic Recursion}

There are several ways to encode polymorphic recursion. We will use type
classes.

PAUSE

\begin{itemize}INCREMENT

\item Standard classes already use polymorphic recursion for deriving instances:
|Show|, |Eq|, etc.

\item The class declaration specifies the type signature.

\item Each recursive case is specified by an instance of the class.

\end{itemize}

PAUSE_LINE

A simplified definition of the |Show| class:

\begin{spec}
class Show a where
  show :: a -> String
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Polymorphic Recursion}

The instances for each structure representation case:

PAUSE_LINE

\begin{columns}
\column{.38\textwidth}
Unit:
\begin{code}
instance Show U where
  show = show_unit
\end{code}
\column{.58\textwidth}
PAUSE
Constructor name:
\begin{code}
instance Show a => Show (C a) where
  show = show_con show
\end{code}
\end{columns}

PAUSE_LINE

Binary product:
\begin{code}
instance (Show a, Show b) => Show (a :*: b) where
  show = show_prod show show
\end{code}

PAUSE_LINE

Binary sum:
\begin{code}
instance (Show a, Show b) => Show (a :+: b) where
  show = show_sum show show
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Polymorphic Recursion}

Now, recall |show_Rep_D|:

\showD

PAUSE_LINE

Compare to the new version that is now possible:
\begin{code}
show_Rep_D' :: Show p => Rep_D' p -> String
show_Rep_D' = show
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Encoding Isomorphisms}

To define the |show| function for |D|, we still need to define another
function:
\begin{code}
show_D' :: Show p => D p -> String
show_D' = show_Rep_D' . from_D'
\end{code}

PAUSE_LINE

Next goal:
\begin{itemize}INCREMENT

\item Define one |show| function that knows how to convert any type |T| to its
structure representation type |Rep_T|, given an isomorphism between |T| and
|Rep_T|.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Encoding Isomorphisms}

We define a class of function pairs.

\begin{itemize}INCREMENT

\item We again use a type class, but with the addition of a \emph{type family}.

\item Each function pair implements an isomorphism between a datatype |T| and
its structure representation |Rep_T|:
\begin{columns}
\column{.28\textwidth}
\begin{spec}
from :: T -> Rep_T
\end{spec}
\column{.28\textwidth}
\begin{spec}
to :: Rep_T -> T
\end{spec}
\end{columns}

\item Each requires two types, so each instance must have two types (unlike the
|Show| instances which needed only the structure representation type).

\item |Rep_T| is precisely determined by |T|, so really we only need one unique
type and a second type derivable from the first.

\item In this case, a (1) multiparameter type class with a functional dependency
and a (2) type class with a type family are equally expressive. (It's a matter
of taste, really.)

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Encoding Isomorphisms}

The type class:
\begin{code}
class Generic a where
  type Rep a
  from  :: a -> Rep a
  to    :: Rep a -> a
\end{code}

PAUSE

\begin{itemize}INCREMENT

\item |Rep| is a type family or, more precisely, an associated type synonym.

\item Think of |Rep| as a function on types. Given a unique type (index) |T|,
you get a type (synonym) |Rep T|.

\item Note that |Rep T| need not be different from |Rep U| even though |T| and
|U| are different.

\item Concretely: two datatypes may have the same representation.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Encoding Isomorphisms}

We need |Generic| instances for every datatype that we want to use with generic
functions.

PAUSE_LINE

The instance for |D| uses definitions that we've already seen:

\begin{code}
instance Generic (D p) where
  type Rep (D p) = Rep_D' p
  from  = from_D'
  to    = to_D'
\end{code}

PAUSE

\begin{itemize}INCREMENT

\item Other instances are defined similarly.

\item In fact, |Rep T|, |from|, and |to| are precisely determined by the
definition of |T|, so these instances can be automatically generated
(e.g.\ using Template Haskell or a preprocessor).

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{The Generic |show| Function}

Finally:
\begin{code}
gshow :: (Show (Rep a), Generic a) => a -> String
gshow = show . from
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{GP in General}

\begin{itemize}INCREMENT

\item Datatype-generic programming:
\begin{itemize}
\item Datatype is the parameter
\item Instantiation gives you a large class of generic functions
\end{itemize}

\item Many generic functions:
\begin{itemize}
\item Pretty-printing (|show|) and parsing (|read|)
\item Compression, serialization, and the reverse
\item Comparison, equality
\item Folds (catamorphisms), unfolds (anamorphisms), maps, zips, zippers
\item Traversals, updates, queries
\end{itemize}

\item Many different libraries:
\begin{itemize}
\item Instant Generics -- presented here
\item Generic Deriving -- GHC \(\geq\) 7.2, similar to Instant Generics
\item EMGM -- maintained by me
\item Regular -- folds, etc.
\item Multirec -- mutually recursive datatypes, folds, etc.
\item Scrap Your Boilerplate (SYB) -- GHC, traversals, queries
\item ...
\end{itemize}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{References}

Generic Programming in Haskell:
\begin{itemize}

\item Johan Jeuring, Sean Leather, Jos\'{e} Pedro Magalh\~{a}es, Alexey
Rodriguez Yakushev. \textit{Libraries for Generic Programming in Haskell}. AFP
2008. pp. 165-229, 2009.

\item Generic Deriving: \url{http://www.haskell.org/haskellwiki/GHC.Generics}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------

\end{document}


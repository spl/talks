\documentclass{beamer}

%-------------------------------------------------------------------------------
% C Preprocessor Directives

#undef DO_PAUSES

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

\newcommand{\CPP}{\ensuremath{\text{C}\plus} }

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
infixr 5 :+:
infixr 6 :*:
data U = U
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

\newcommand{\FormatClass}{%
\begin{code}
class Apply (F f) => Format f where
  type F f :: * -> *
  showf' :: f -> F f String
\end{code}
}

\newcommand{\showfFunction}{%
\begin{code}
showf :: Format f => f -> A (F f) String
showf = apply . showf'
\end{code}
}

%if style == newcode
\begin{code}
data StringF = String

instance Format StringF where
  type F StringF = Arr String
  showf' String = Arr id

data a :%: b = a :%: b

infixr 8 :%:

(%) :: a -> b -> a :%: b
(%) = (:%:)

infixr 8 %

instance (Format f, Format g) => Format (f :%: g) where
  type F (f :%: g) = F f :.: F g
  showf' (f :%: g) = showf' f <> showf' g

data PrecF a = Prec Int

instance Real a => Format (PrecF a) where
  type F (PrecF a) = Arr a
  showf' (Prec i)
    | i < 0 =
      error $ "showf': bad precision: " ++ show i
    | otherwise =
      Arr (TP.printf ("%." ++ show i ++ "f") . toDouble)
        where
          toDouble :: Real a => a -> Double
          toDouble = realToFrac

data FillF f = Fill Dir Bool Char Int f
data Dir = L_ | R_

fill :: Dir -> Bool -> Char -> Int -> String -> String
fill dir doChop ch wid input =
  case dir of
    L_ -> chop (drop (len - wid)) $ excess ++ input
    R_ -> chop (take wid)         $ input ++ excess
  where
    len = length input
    chop act = if doChop && len > wid then act else id
    excess | len < wid = replicate (wid - len) ch
           | otherwise = ""

instance Format f => Format (FillF f) where
  type F (FillF f) = F f
  showf' (Fill dir chp fil wid f) = fmap (fill dir chp fil wid) (showf' f)

fillL :: Int -> f -> FillF f
fillL = Fill L_ False ' '

fillL' :: Int -> f -> FillF f
fillL' = Fill L_ True ' '

fillR :: Int -> f -> FillF f
fillR = Fill R_ False ' '

fillR' :: Int -> f -> FillF f
fillR' = Fill R_ True ' '

zero :: Int -> f -> FillF f
zero = Fill L_ False '0'

zero' :: Int -> f -> FillF f
zero' = Fill L_ True '0'

instance (Format f1, Format f2, Format f3) => Format (f1, f2, f3) where
  type F (f1, f2, f3) = F f1 :.: F f2 :.: F f3
  showf' (f1, f2, f3) = showf' f1 <> showf' f2 <> showf' f3
instance (Format f1, Format f2, Format f3, Format f4) => Format (f1, f2, f3, f4) where
  type F (f1, f2, f3, f4) = F f1 :.: F f2 :.: F f3 :.: F f4
  showf' (f1, f2, f3, f4) = showf' f1 <> showf' f2 <> showf' f3 <> showf' f4
\end{code}
%endif

%-------------------------------------------------------------------------------
% Title

\title[GP in Haskell]{Generic Programming \\ in Haskell}
\subtitle{Presented to the Lambda Luminaries}

\author{Sean Leather}

\institute{Utrecht University}

\date[2012-06-21]{21 June 2012}

%-------------------------------------------------------------------------------

\begin{document}

%-------------------------------------------------------------------------------
\begin{frame}

\titlepage

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{What is \alert{Generic Programming}?}

PAUSE

The adjective ``generic'' is heavily overloaded.

PAUSE_LINE

\begin{itemize}INCREMENT
\item Java/C\# generics
\item C++ templates
\item Ada generic packages
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{What is Generic Programming?}

\begin{block}{The goal is often the same.}
A higher level of abstraction than ``normally'' available
\end{block}

PAUSE_LINE

\begin{block}{The technique is also often the same.}
Some form of parameterization and instantiation
\end{block}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}[fragile]{Examples of Generic Programming}

Java/C\#:
\begin{beamerboxesrounded}{}
\begin{lstlisting}[language=Java]
public class Stack<T>
{
  public void push(T item) {...}
  public T pop() {...}
}
\end{lstlisting}
\end{beamerboxesrounded}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}[fragile]{Examples of Generic Programming}

\CPP:
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

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Generic Programming in Haskell}

In other words:
\begin{itemize}INCREMENT
\item Java-style generics \(\approx\) parametric polymorphism
\item C++ templates \(\approx\) ad-hoc polymorphism
\end{itemize}

PAUSE_LINE

In Haskell:
\begin{itemize}INCREMENT
\item Both forms already exist.
\item We don't call them generics because they're native to the language.
\end{itemize}

PAUSE_LINE

\alert{Datatype-generic programming}:
\begin{itemize}INCREMENT
\item Abstract over the \emph{structure of a datatype}
\item Also known as ``polytypism'' and ``shape-/structure-polymorphism''
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Datatypes}

\dataD

PAUSE_LINE

A datatype can have:
\begin{itemize}INCREMENT
\item \PURPLE{Parameters}: type variables (\(\geq 0\))
\item \GREEN{Alternatives}: unique constructors (\(\geq 0\))
\item \BLUE{Fields}: types for each constructor (\(\geq 0\))
\end{itemize}

PAUSE_LINE

Non-syntactic features:
\begin{itemize}INCREMENT
\item Recursion
\item Nesting
\end{itemize}

PAUSE_LINE

There are other features of datatypes, but we will consider only the above as a
foundation for looking at the structure.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure of Datatypes: Sums}

%if style /= newcode
%format AltEx_2
%format AltEx_2' = "\Conid{{AltEx}_{2}^{\prime}}"
%format AltEx_3
%format AltEx_3' = "\Conid{{AltEx}_{3}^{\prime}}"
%format A_1
%format A_2
%format a_1
%format a_2
%endif

First structural element: alternatives.
\begin{code}
data AltEx_2 = (GREEN(A_1)) (BLUE(Int)) | (GREEN(A_2)) (BLUE(Char))
\end{code}

PAUSE_LINE

Note that the above is similar to a standard type:
\begin{spec}
data Either (PURPLE(a)) (PURPLE(b)) = (GREEN(Left)) (BLUE(a)) | (GREEN(Right)) (BLUE(b))
\end{spec}

PAUSE_LINE

And we can, in fact, model |AltEx_2| as:
\begin{code}
type AltEx_2' = Either (BLUE(Int)) (BLUE(Char))
\end{code}

with the following ``smart'' constructors:

\begin{columns}
\column{.48\textwidth}
\begin{code}
a_1 :: Int -> AltEx_2'
a_1 = Left
\end{code}
\column{.48\textwidth}
\begin{code}
a_2 :: Char -> AltEx_2'
a_2 = Right
\end{code}
\end{columns}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure of Datatypes: Sums}

%if style /= newcode
%format B_1
%format B_2
%format B_3
%format b_1
%format b_2
%format b_3
%endif

When talking about alternatives in structural sense, we often call them
\alert{sums}. |Either| is the basic binary sum type. For conciseness, we use
this (identical) binary sum type:

\begin{code}
data (PURPLE(a)) :+: (PURPLE(b)) = (GREEN(L)) (BLUE(a)) | (GREEN(R)) (BLUE(b))
\end{code}

PAUSE_LINE

What about a type with \(< 2\) alternatives?
\begin{code}
data AltEx_3 = (GREEN(B_1)) (BLUE(Int)) | (GREEN(B_2)) (BLUE(Char)) | (GREEN(B_3)) (BLUE(Float))
\end{code}

PAUSE_LINE

The simplest solution is to nest one binary sum inside another:
\begin{code}
type AltEx_3' = (BLUE(Int)) :+: ((BLUE(Char)) :+: (BLUE(Float)))
\end{code}

Note that:
\begin{code}
b_3 :: Float -> AltEx_3'
b_3 = R . R
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure of Datatypes: Products}

%if style /= newcode
%format FldEx_2
%format FldEx_2' = "\Conid{{FldEx}_{2}^{\prime}}"
%format fldEx_2' = "\Varid{{fldEx}_{2}^{\prime}}"
%format FldEx_3
%format FldEx_3' = "\Conid{{FldEx}_{3}^{\prime}}"
%format fldEx_3' = "\Varid{{fldEx}_{3}^{\prime}}"
%endif

Next: fields.
\begin{code}
data FldEx_2 = (GREEN(FldEx_2)) (BLUE(Int)) (BLUE(Char))
\end{code}

PAUSE_LINE

Again, note the similarity to a standard type, the pair:
\begin{spec}
data (,) (PURPLE(a)) (PURPLE(b)) = (GREEN((,))) (BLUE(a)) (BLUE(b))
\end{spec}

PAUSE_LINE

And again, we model |FldEx_2| similarly:
\begin{code}
type FldEx_2' = (,) (BLUE(Int)) (BLUE(Char))
\end{code}

with the smart constructor:
\begin{code}
fldEx_2' :: (BLUE(Int)) -> (BLUE(Char)) -> FldEx_2'
fldEx_2' = (,)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure of Datatypes: Products}

The pair type is the basic binary \alert{product} type. For symmetry with sums,
we will use the following type:

\begin{code}
data (PURPLE(a)) :*: (PURPLE(b)) = (BLUE(a)) :*: (BLUE(b))
\end{code}

PAUSE_LINE

And more than two fields...
\begin{code}
data FldEx_3 = (GREEN(FldEx_3)) (BLUE(Int)) (BLUE(Char)) (BLUE(Float))
\end{code}

PAUSE

... are modeled by nested binary products:
\begin{code}
type FldEx_3' = (BLUE(Int)) :*: ((BLUE(Char)) :*: (BLUE(Float)))
\end{code}

with the smart constructor:
\begin{code}
fldEx_3' :: (BLUE(Int)) -> (BLUE(Char)) -> (BLUE(Float)) -> FldEx_3'
fldEx_3' x y z = x :*: (y :*: z)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure of Datatypes: Sums of Products}

To ``sum'' it all up, recall the first datatype example:

\dataD

PAUSE_LINE

We can define an identical type using the sum and product types we have
just discussed:
\begin{code}
type Rep_D (PURPLE(p)) = (BLUE(U)) :+: (BLUE(Int)) :*: (BLUE(p))
\end{code}

PAUSE_LINE

Notes:
\begin{itemize}INCREMENT

\item We use the ``unit'' type |data U = U| (identical to the standard type
|()|) to represent an alternative without fields.

\item |:+:| is |infixr 5|, and |:*:| is |infixr 6|, so we can write |Rep_D|
naturally, without unnecessary parentheses.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure of Datatypes: Isomorphism}

%if style /= newcode
%format from_D = "\Varid{from_{" D "}}"
%format from_D' = "\Varid{from_{" D "}}"
%format to_D = "\Varid{to_{" D "}}"
%format to_D' = "\Varid{to_{" D "}}"
%endif

So, we think we can model datatypes. But how do we know |Rep_D| accurately
models |D|?

PAUSE_LINE

We define an \alert{isomorphism}: two total functions that convert between
types.

PAUSE

\begin{code}
from_D :: D p -> Rep_D p
from_D  Alt_1          = L U
from_D  (Alt_2 i p)    = R (i :*: p)

to_D :: Rep_D p -> D p
to_D    (L U)          = Alt_1
to_D    (R (i :*: p))  = Alt_2 i p
\end{code}

PAUSE

This allows us to convert terms between (1) the familiar datatype and (2) the
\alert{structure representation} used for generic operations.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Structure of Datatypes: Constructors}

Oh, but there's one more thing...

PAUSE

You may have noticed the representation lacked any information about the
constructors (e.g.\ the names).

PAUSE_LINE

That's easily repaired with another datatype:
\begin{code}
data C (PURPLE(a)) = C String (BLUE(a))
\end{code}

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
\begin{frame}{Generic Functions}

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
\begin{frame}{Generic Functions: |show|}

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
\begin{frame}{Generic Functions: |show|}

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
\begin{frame}{Generic Functions: |show|}

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
\begin{frame}{Generic Functions, Generically}

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
\begin{frame}{Polymorphic Recursion}

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
\begin{frame}{Polymorphic Recursion}

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
\begin{frame}{Polymorphic Recursion}

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
\begin{frame}{Encoding Isomorphisms}

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
\begin{frame}{Encoding Isomorphisms}

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
\begin{frame}{Encoding Isomorphisms}

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
\begin{frame}{Encoding Isomorphisms}

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
\begin{frame}{The Generic |show| Function}

Finally:
\begin{code}
gshow :: (Show (Rep a), Generic a) => a -> String
gshow = show . from
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{GP in General}

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
\begin{frame}{A Different Application}

\begin{itemize}INCREMENT

\item Now, I want to present a different approach to using the structure of
datatypes.

\item This applies to the HollingBerries specification.

\item The problem we look at is |printf|.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}[fragile]{Presenting |printf|}

In C and related languages, we have a function such as:
\begin{beamerboxesrounded}{}
\begin{lstlisting}[language=C]
int printf(const char *format, ...)
\end{lstlisting}
\end{beamerboxesrounded}

PAUSE

The following code works:
\begin{beamerboxesrounded}{}
\begin{lstlisting}[language=C]
printf("%s W%drld!\n", "Hello", 0); // Hello W0rld!
\end{lstlisting}
\end{beamerboxesrounded}

PAUSE

But so does the following:
\begin{beamerboxesrounded}{}
\begin{lstlisting}[language=C]
printf("%s W%drld!\n", 0, "Hello"); // (null) W134514152rld!
\end{lstlisting}
\end{beamerboxesrounded}

PAUSE

As does this:
\begin{beamerboxesrounded}{}
\begin{lstlisting}[language=C]
printf("%s W%drld!\n", "Hello"); // Hello W134514152rld!
\end{lstlisting}
\end{beamerboxesrounded}

PAUSE

And we can't do this (where |Y| is a new format descriptor):
\begin{beamerboxesrounded}{}
\begin{lstlisting}[language=C]
printf("%Y\n", x); // Y
\end{lstlisting}
\end{beamerboxesrounded}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}[fragile]{Problems With |printf|}

\begin{itemize}INCREMENT
\item No type-checking of arguments
\item No arity check
\item Restricted set of format descriptors
\begin{itemize}
\item Not extensible
\end{itemize}
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{|Text.Printf|}

Is this the solution in Haskell?
\begin{spec}
printf :: PrintfType r => String -> r
\end{spec}

\begin{itemize}INCREMENT
\item From |Text.Printf| in \pkg{base}
\item Abstract type class |PrintfType|
\item |String| format descriptor
\end{itemize}

PAUSE
Try it in GHCi:
\begin{verbatim}
ghci> printf "%s W%drld!\n" "Hello" 0
\end{verbatim}
PAUSE
\begin{verbatim}
ghci> printf "%s W%drld!\n" 0 "Hello"
\end{verbatim}
PAUSE
\begin{verbatim}
ghci> printf "%s W%drld!\n" "Hello"
\end{verbatim}

PAUSE

Apparently, |Text.Printf| is not the solution.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{\pkg{xformat}}

The solution:
\begin{spec}
printf :: Format f => f -> A (F f) (IO ())
\end{spec}

\begin{itemize}INCREMENT
\item From |Text.XFormat.Show| in \pkg{xformat}
\item |f| is the format descriptor
\item |A| and |F| are type families
\item Also: |showf :: Format f => f -> A (F f) String|
\item |IO ()| (or |String|) is the result type
\end{itemize}

PAUSE
Try it in GHCi:
\begin{verbatim}
ghci> printf (String, " W", Int, "rld!\n") "Hello" 0
\end{verbatim}
PAUSE
\begin{verbatim}
ghci> printf (String, " W", Int, "rld!\n") 0 "Hello"
\end{verbatim}
PAUSE
\begin{verbatim}
ghci> printf (String, " W", Int, "rld!\n") "Hello"
\end{verbatim}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Application to HollingBerries}

%if style == newcode
\begin{code}
price :: Float
price = 23.02
year = 2012
month = 6
day = 21
desc = "blah"
ex1 :: IO ()
\end{code}
%endif

|Text.Printf|:
\begin{code}
(HIDEDEF(ex1)(TP.printf "R%8.2f%d/%02d/%02d%-31s\n" price year month day desc))
\end{code}

PAUSE_LINE

|Text.XFormat.Show|:
PAUSE
\begin{code}
priceF = ("R", fillL 8 (Prec 2))
\end{code}
PAUSE
\begin{code}
dateF = Int % "/" % zero 2 Int % "/" % zero 2 Int
\end{code}
PAUSE
\begin{code}
descF = fillR' 31 String
\end{code}
PAUSE
\begin{code}
(HIDEDEF(ex2)(printf (priceF, dateF, descF, "\n") price year month day desc))
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Advantages of XFormat}

Just from this example, there are several obvious advantages:
PAUSE
\begin{itemize}INCREMENT
\item Type-safe: see types in GHCi
\item Computable
\begin{itemize}
\item Compute numbers (e.g.\ column widths) instead of strings
\end{itemize}
\item Modular and composable
\begin{itemize}
\item Reuse formats in different combinations
\end{itemize}
\end{itemize}

PAUSE_LINE

There is also a non-obvious advantage:
\begin{itemize}
\item Flexible
\begin{itemize}
\item Use any |Real| for |Prec| (e.g.\ |Int|, |Float|, or |Double|)
\end{itemize}
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Behind the Scenes}

How does \pkg{xformat} work?
PAUSE
\begin{itemize}INCREMENT
\item Polyvariadic functions
\begin{itemize}
\item Variable number of arguments (of different types)
\end{itemize}
\item Format descriptors may...
\begin{itemize}
\item Require an argument:
\begin{code}
(HIDEDEF(ex3)(printf String "Sharp!"))
\end{code}
\item Not allow any argument:
\begin{code}
(HIDEDEF(ex4)(printf "Yebo."))
\end{code}
\item Compose for multiple arguments (or not):
\begin{code}
(HIDEDEF(ex5)(printf (String, " yebo ", String) "Ja" "yes!"))
\end{code}
\end{itemize}
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Polyvariadic Functions}

%if style /= newcode
%format T_1
%format T_n
%format f_1
%format f_n
%endif

\begin{itemize}INCREMENT

\item In the end, we construct a function, |T_1 -> ... -> T_n|, given a
composition, |f_1 <> ... <> f_n|, of format descriptors.

\item We track the argument (and result) type expected by each descriptor using
functors.

\item We combine multiple descriptors using functor composition.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Polyvariadic Functions}

\begin{itemize}INCREMENT

\item Identity -- no argument
\begin{code}
newtype Id a = Id a
\end{code}

\item Arrow -- one argument
\begin{code}
newtype Arr a b = Arr (a -> b)
\end{code}

\item Composition -- combining
\begin{code}
newtype (:.:) f g a = Comp (f (g a))
infixr 8 :.:
\end{code}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Polyvariadic Functions}

The |Functor| instances:
\begin{code}
instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Functor (Arr a) where
  fmap f (Arr g) = Arr (f . g)

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp fga) = Comp (fmap (fmap f) fga)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Polyvariadic Functions}

\begin{itemize}INCREMENT

\item The functors give us terms for the components of a function composition.

\item We need a way to ``lift'' the functor to get the actual function type.

\end{itemize}

PAUSE

\begin{code}
class Functor f => Apply f where
  type A f a
  apply :: f a -> A f a
\end{code}

\begin{itemize}INCREMENT
\item |A| is the polyvariadic function type derived from the functor |f|.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Polyvariadic Functions}

The |Apply| instances:
\begin{code}
instance Apply Id where
  type A Id a = a
  apply (Id x) = x
\end{code}
PAUSE
\begin{code}
instance Apply (Arr a) where
  type A (Arr a) b = a -> b
  apply (Arr f) = f
\end{code}
PAUSE
\begin{code}
instance (Apply f, Apply g) => Apply (f :.: g) where
  type A (f :.: g) a = A f (A g a)
  apply (Comp fg) = apply (fmap apply fg)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Polyvariadic Functions}

Resolve these types in GHCi (using \texttt{kind!} from \(\geq 7.4\)):
%if style == newcode
\begin{code}
data T1 = T1
data T2 = T2
data T3 = T3
\end{code}
%endif
\begin{code}
(HIDEDEF(ex6)(apply (Id T1) :: (RED(A Id T1))))
\end{code}
PAUSE
\begin{code}
(HIDEDEF(ex7)(apply (Arr (\T1 -> T2)) :: (RED(A (Arr T1) T2))))
\end{code}
PAUSE
\begin{code}
(HIDEDEF(ex8)(apply (Comp (Arr (\T1 -> Arr (\T2 -> T3)))) :: (RED(A (Arr T1 :.: Arr T2) T3))))
\end{code}
PAUSE
\begin{code}
(HIDEDEF(ex9a)(apply (Comp (Arr (\T1 -> Id T2)))    :: (RED(A (Arr T1 :.: Id) T2))))
(HIDEDEF(ex9b)(apply (Comp (Id (Arr (\T1 -> T2))))  :: (RED(A (Id :.: Arr T1) T2))))
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Format Functors}

With the help of...
\begin{code}
(<>) :: (Functor f, Functor g) => f String -> g String -> (:.:) f g String
f <> g = Comp (fmap (\s -> fmap (\t -> s ++ t) g) f)
infixr 8 <>
\end{code}

PAUSE
... we can easily compose functors, ...
\begin{code}
wrldF :: Show a => (Arr String :.: Id :.: Arr a :.: Id) String
wrldF = Arr id <> Id " W" <> Arr show <> Id "rld!\n"
\end{code}

PAUSE
... lift the resulting functor to a function type, ...
\begin{code}
wrld :: Show a => String -> a -> String
wrld = apply wrldF
\end{code}

PAUSE
... and greet the w0rld.
\begin{verbatim}
ghci> putStr $ wrld "Hello" 0
Hello W0rld!
\end{verbatim}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Format Descriptors}

But this isn't quite good enough.

PAUSE_LINE

We can go a step further by using meaningful descriptors instead of |Id|, |Arr|,
and |Comp|.

PAUSE_LINE

Introducing the |Format| class:
\FormatClass

PAUSE_LINE

And the |showf| function:
\showfFunction

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Format Descriptors}

\FormatClass

\begin{itemize}INCREMENT

\item Descriptors are instances of |Format|

\item Some instances define primitives:
\begin{code}
instance Format String where
  type F String = Id
  showf' s = Id s
\end{code}

\item Some instances define argument types:
\begin{code}
data IntF = Int

instance Format IntF where
  type F IntF = Arr Int
  showf' Int = Arr show
\end{code}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Format Descriptors}

%if style /= newcode
%format f_1
%format f_2
%endif

\FormatClass

\begin{itemize}INCREMENT

\item Some instances define recursive descriptors:
\begin{code}
instance (Format f_1, Format f_2) => Format (f_1, f_2) where
  type F (f_1, f_2) = F f_1 :.: F f_2
  showf' (f_1, f_2) = showf' f_1 <> showf' f_2
\end{code}

\item And yet other instances are even more interesting. See code!

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Format Descriptors}

\begin{itemize}INCREMENT

\item Recall |showf|:
\showfFunction

\item How do we define |printf|?

\item Note that we cannot simply do |putStr . showf|:
\begin{verbatim}
ghci> :t putStr . showf
putStr . showf
  :: (Format a, A (F a) String ~ String) => a -> IO ()
\end{verbatim}

\item But |F f| is a |Functor|:
\begin{verbatim}
ghci> :t fmap putStr . showf'
fmap putStr . showf' :: Format a => a -> F a (IO ())
\end{verbatim}

\item So, we can now define |printf|:
\begin{code}
printf :: Format f => f -> A (F f) (IO ())
printf = apply . fmap putStr . showf'
\end{code}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{Summary of \pkg{xformat}}

\begin{itemize}INCREMENT

\item This description is based on |Text.XFormat.Show|:

\begin{itemize}
\item One difference: package uses the more efficient |String -> String| instead
of |String|.
\end{itemize}

\item Also |Text.XFormat.Read|:
\begin{spec}
readf :: Format f => f -> String -> Maybe (R f)
\end{spec}

\begin{itemize}
\item |R| is a type family that determines the structure of the result from the
format descriptor |f|.
\item No functors involved. Simpler.
\end{itemize}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}{References}

Generic Programming in Haskell:
\begin{itemize}

\item Johan Jeuring, Sean Leather, Jos\'{e} Pedro Magalh\~{a}es, Alexey
Rodriguez Yakushev. \textit{Libraries for Generic Programming in Haskell}. AFP
2008. pp. 165-229, 2009.

\item Generic deriving: \url{http://www.haskell.org/haskellwiki/GHC.Generics}

\end{itemize}

\pkg{xformat}:
\begin{itemize}

\item Olivier Danvy. \textit{Functional Unparsing}. Journal of Functional
Programming. v8, N6, pp. 621-625. 1998.

\item Ralf Hinze. \textit{Formatting: A Class Act}. Journal of Functional
Programming. v13, N5, pp. 935-944. 2003.

\item Oleg Kiselyov: \url{http://okmij.org/ftp/typed-formatting/FPrintScan.html}

\item More: \url{http://www.citeulike.org/user/spl/tag/printf}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------

\end{document}


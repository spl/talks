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
-- Initial
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

-- Extended
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
module Talk where
import Prelude hiding (Show(show))
\end{code}
%endif

%if style /= newcode
%format show_unit = "\Varid{show_{U}}"
%format show_con = "\Varid{show_{C}}"
%format show_k = "\Varid{show_{K}}"
%format show_prod = "\Varid{show_{\times}}"
%format show_sum = "\Varid{show_{+}}"
%format show_int = "\Varid{show_{Int}}"
%format show_char = "\Varid{show_{Char}}"
%format show_ = "\Varid{show_{?}}"
%format show_a
%format show_b
%format Rep_E
%format show_Rep_E = "\Varid{show_{" Rep_E "}}"
%format show_Rep_E' = "\Varid{show_{" Rep_E "}}"
%format show_E
%endif

%if style == newcode
\begin{code}
show_int :: Int -> String
show_int = show
instance Show Int where
  show = show_int

show_char :: Char -> String
show_char = show
instance Show Char where
  show = show_char
\end{code}
%endif

%if style == newcode
\begin{code}
instance Uniplate (E a)
instance Fold (E a)
\end{code}
%endif

\newcommand{\RepE}{%
\begin{code}
type Rep_E a = (GREEN(C)) U :+: (GREEN(C)) ((BLUE(K)) a :*: (BLUE(K)) (E a) :*: (BLUE(K)) Int)
\end{code}
}

\newcommand{\showRepE}{%
\begin{code}
show_Rep_E  ::  (a -> String) -> ((a -> String) -> E a -> String)
            ->  Rep_E a -> String

show_Rep_E show_a show_E =
  show_sum  (show_con  show_unit)
            (show_con  (show_prod  (show_k show_a)
                                   (show_prod (show_k (show_E show_a)) (show_k show_int))))
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
\item Uniplate -- similar to SYB but faster and less expressive
\item EMGM -- fast sums-of-products
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

\item[Fixed-point] A datatype is a sums-of-products with recursive points.
\\ Example: Multirec

\end{description}

PAUSE_LINE

We will look at depth into sums-of-products.

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
%endif

\begin{code}
data T_prod = (GREEN(P)) (BLUE(Char)) (BLUE(Int))
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

Note: There are other features of datatypes, but we consider only the above.

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
\RepE{}

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
from_E  E_1          = L  (C "E1" U)
from_E  (E_2 x e i)  = R  (C "E2" ((K x) :*: (K e) :*: (K i)))
\end{code}

PAUSE_LINE

\begin{code}
to_E :: Rep_E a -> E a
to_E  (L  (C "E1" U))                            = E_1
to_E  (R  (C "E2" ((K x) :*: (K e) :*: (K i))))  = E_2 x e i
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Converting Between Types: Isomorphism}

For convenience, we join the representation type and isomorphism in a type class
|Generic| with an associated type synonym |Rep|.
\begin{code}
class Generic a where
  type Rep a
  from  :: a -> Rep a
  to    :: Rep a -> a
\end{code}

PAUSE_LINE

The instance for |E|:
\begin{code}
instance Generic (E a) where
  type Rep (E a) = Rep_E a
  from  = from_E
  to    = to_E
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Generic Functions}

A \alert{generic function}
\begin{itemize}INCREMENT

\item Is defined on each case of the structure representation and

\item Works for every datatype that has a structure representation and
isomorphism.

\end{itemize}

PAUSE_LINE

%if style /= newcode
%format show_Rep_a = "\Varid{show_{Rep\ a}}"
%endif

Example: |show_Rep_a :: a -> String|

\begin{itemize}
\item We define a |show| function for each case.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |show|}

Unit:
\begin{code}
show_unit :: U -> String
show_unit U = ""
\end{code}

PAUSE_LINE

Constructor name:
\begin{code}
show_con :: (a -> String) -> C a -> String
show_con show_a (C nm a) = "(" ++ nm ++ " " ++ show_a a ++ ")"
\end{code}

PAUSE_LINE

Field:
\begin{code}
show_k :: (a -> String) -> K a -> String
show_k show_a (K a) = show_a a
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |show|}

Binary sum:
\begin{code}
show_sum :: (a -> String) -> (b -> String) -> a :+: b -> String
show_sum show_a _ (L  a)  = show_a  a
show_sum _ show_b (R  b)  = show_b  b
\end{code}

PAUSE_LINE

Binary product:
\begin{code}
show_prod :: (a -> String) -> (b -> String) -> a :*: b -> String
show_prod show_a show_b (a :*: b) = show_a a ++ " " ++ show_b b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |show|}

Recall:
\RepE{}

LINE

We can define a |show| function (assuming |show_int|):
\showRepE{}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |show|}

\showRepE{}

LINE

The |show_E| function itself is just an isomorphism away:
\begin{code}
show_E :: (a -> String) -> E a -> String
show_E show_a = show_Rep_E show_a show_E . (PURPLE(from_E))
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |show|}

\showRepE{}

LINE

Some observations:
\begin{itemize}INCREMENT

\item This is \textbf{not} a generic function.

\item It is defined on the structure of |E|, not on datatypes in general.

\item It demonstrates a predictable pattern for defining the generic function.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |show|}

Consider these typical expressions and their types:
\begin{spec}
show_con   show_unit                             :: C U                 -> String
show_prod  (show_k show_int) (show_k show_char)  :: (K Int :*: K Char)  -> String
\end{spec}

\begin{itemize}INCREMENT

\item |show_| functions call other |show_| functions.

\item They can be considered recursive but not in the usual way.

\item \alert{Polymorphic recursion} -- functions with different types that have
a common scheme that reference each other

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |show|}

There are several ways to encode polymorphic recursion. We use type classes.

PAUSE

\begin{itemize}INCREMENT

\item The class declaration specifies the type signature.

\item Each recursive (type) case is specified by an instance of the class.

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
\frametitle{Defining |show|}

Some of the instances for each structure representation case:

PAUSE_LINE

Constructor name:
\begin{code}
instance Show a => Show (C a) where
  show = show_con show
\end{code}

PAUSE_LINE

Binary sum:
\begin{code}
instance (Show a, Show b) => Show (a :+: b) where
  show = show_sum show show
\end{code}

PAUSE_LINE

The remaining instances are straightforward.

%if style == newcode
Unit:
\begin{code}
instance Show U where
  show = show_unit
\end{code}

Field:
\begin{code}
instance Show a => Show (K a) where
  show = show_k show
\end{code}

Binary product:
\begin{code}
instance (Show a, Show b) => Show (a :*: b) where
  show = show_prod show show
\end{code}
%endif

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |show|}

Now, compare:
\showRepE{}

PAUSE_LINE

To:
\begin{code}
show_Rep_E' :: (Show a, Show (E a)) => Rep_E a -> String
show_Rep_E' = show
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |show|}

Finally, we can use a slightly different |Show| class to support generic
functions for any type that has a representation.

PAUSE

\begin{code}
class Show a where

  show :: a -> String

  default show :: (Generic a, Show (Rep a)) => a -> String
  show = show . from
\end{code}
\begin{itemize}

\item This uses the |DefaultSignatures| language extension: if type |a| has the
instances |Show (Rep a)| and |Generic a|, then the given definition is used.

\end{itemize}

PAUSE_LINE

The instance for |E|:
\begin{code}
instance Show a => Show (E a)
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Sums-of-Products and Beyond}

We presented a sums-of-products view.
\begin{itemize}INCREMENT
\item We used Haskell2010 plus a few GHC language extensions:
\begin{spec}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
\end{spec}
\item Typically, a GP library does not support another view.
\item But we can, with a few more extensions:
\begin{spec}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
\end{spec}
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{The Uniplate View}

\begin{itemize}INCREMENT

\item Uniplate uses a simplified spine view.

\item The spine is the sequence of fields in a constructor.

\item SYB models the ``full'' spine, i.e.\ all fields (which can naturally have
different types).

\item Uniplate models only a list of the (recursive) children (which have the
same type).

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |descend|}

\begin{itemize}INCREMENT

\item We define the function |descend| from Uniplate to demonstrate that our
library can model the simplified spine view.

\item |descend| performs a traversal of the children and applies a function to
each one.

\item We use the following signature:
\begin{spec}
class Uniplate a where
  descend :: (a -> a) -> a -> a
\end{spec}

\item Note that we must traverse every field to determine whether that field is
a child or not. (Uniplate does this in an ad-hoc way.)

\item Our generic function must support:
\begin{itemize}
\item Polymorphic recursion on the structure (as usual) \textit{and}
\item A function parameter whose type matches only some of the fields.
\end{itemize}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |descend'|}

Consequently, we use a signature with different types for the function
parameter and the structure representation:
\begin{code}
class Uniplate' a r where
  descend' :: (r -> r) -> a -> a
\end{code}
PAUSE
\begin{itemize}INCREMENT
\item We need the function parameter type (|r|) in the |Uniplate'| head.
\item We will come back to |Uniplate| later.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |descend'|}

Most of the instances are straightforward:
\begin{code}
instance Uniplate' U a where
  descend' _ U = U
\end{code}
PAUSE
\begin{code}
instance Uniplate' a r => Uniplate' (C a) r where
  descend' f (C nm a) = C nm (descend' f a)
\end{code}
PAUSE
\begin{code}
instance (Uniplate' a r, Uniplate' b r) => Uniplate' (a :+: b) r where
  descend' f (L  a)  = L  (descend' f a)
  descend' f (R  b)  = R  (descend' f b)
\end{code}
PAUSE
\begin{code}
instance (Uniplate' a r, Uniplate' b r) => Uniplate' (a :*: b) r where
  descend' f (a :*: b) = descend' f a :*: descend' f b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |descend'|}

It is the |K| instance that is interesting.

PAUSE_LINE

Because there is a fall-back instance:
\begin{code}
instance Uniplate' (K a) r where
  descend' _ (K a) = K a
\end{code}

PAUSE_LINE

And an instance where we apply the function parameter:
\begin{code}
instance Uniplate' (K (PURPLE(a))) (PURPLE(a)) where
  descend' f (K a) = K ((PURPLE(f)) a)
\end{code}
\begin{itemize}INCREMENT

\item Note the matching types |a| in the head.

\item Overlapping instances implies type equality.

\item This is the ``trick'' that allows us to determine when to choose this
instance.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |descend|}

Coming back to an improved |Uniplate| class:
\begin{code}
class Uniplate a where
  descend :: (a -> a) -> a -> a

  default descend :: (Generic a, Uniplate' (Rep a) a) => (a -> a) -> a -> a
  descend f = to . descend' f . from
\end{code}
\begin{itemize}INCREMENT

\item We again use |DefaultSignatures| to simplify instantiation.

\item The types of the function parameter and generic parameter are the same.

\item They only differ ``behind the scenes.''

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Uniplate View and Beyond}

We presented a traversal function
\begin{itemize}INCREMENT
\item From Uniplate (which is not a sums-of-products library)
\item In a library with a sums-of-products view
\item Extended with overlapping instances (and type equality in particular).
\end{itemize}

PAUSE_LINE

With a bit more work, we can also define functions that work on all fields and
not just the recursive children, e.g.:
\begin{spec}
topDown :: C b a => (a -> a) -> b -> b
\end{spec}
\begin{itemize}INCREMENT

\item For a class |C| that supports matching on any type |T| for which there is
an instance |C T T|

\item Similar to the function |everywhere'| in SYB

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{The Fixed-Point View}

\begin{itemize}INCREMENT

\item We can use the type equality trick to model the fixed-point view in our
library.

\item The fixed-point view typically extends the sums-of-products view with an
explicit indicator of recursive points in the structure.

\item In the basic sums-of-products view, recursion occurs on the structure but
not on the datatype.

\item In the basic fixed-point view, we define one case of a generic function
on the recursive points structural element.

\item In our library, we pass the top-level type |T| through the type cases.

\item The case at which we can match on |T| is the recursive point.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |fold|}

\begin{itemize}INCREMENT

\item We define the function |fold| (catamorphism).

\item |fold| iterates from the root of a value to its leaves and builds up a
new result based on the recursive structure of the input.

\item We use the following signature:
\begin{spec}
class Fold a where
  fold :: Alg (Rep a) r -> a -> r
\end{spec}

\item Given an algebra and a value, compute the result of applying the algebra
to the structure of the value.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |Alg|}

The algebra of the fold is a type family:
\begin{code}
type family Alg a r

type instance Alg U             r = r
type instance Alg (K a)         r = Either a r -> r
type instance Alg (C a)         r = Alg a r
type instance Alg (a :+: b)     r = (Alg a r, Alg b r)
type instance Alg (K a :*: b)   r = Either a r -> Alg b r
\end{code}
\begin{itemize}INCREMENT
\item |Alg| is indexed on the representation type of the input type |a|.
\item The type |r| is the result of the fold.
\item |K| types can be either non-recursive (|a|) or recursive (|r|) points.
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |Alg|}

For the example type:
\RepE{}

The algebra type is:
\begin{spec}
type instance Alg (Rep (E a)) r =
  (r, Either a r -> Either (E a) r -> Either Int r -> r)
\end{spec}
\begin{itemize}INCREMENT

\item |E a| is the recursive point, even though it does not appear so in the
type.

\item The instances of the generic function ensure the separation of
non-recursive and recursive |K| cases.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |Fold'|}

We again define a helper generic function:
\begin{code}
class Fold' a t where
  fold' :: proxy t -> Alg (Rep t) r -> Alg a r -> a -> r
\end{code}
\begin{itemize}INCREMENT

\item |a| is the structure type.

\item |t| is the recursive type.

\item The ``proxy'' provides proof of |t| while preventing the instances of
|Fold'| from using it.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |Fold'|}

The instances that do not have recursion:
\begin{code}
instance Fold' U t where
  fold' _ _ alg U = alg
\end{code}
\begin{code}
instance Fold' a t => Fold' (C a) t where
  fold' p palg alg (C _ a) = fold' p palg alg a
\end{code}
\begin{code}
instance (Fold' a t, Fold' b t) => Fold' (a :+: b) t where
  fold' p palg (alg, _) (L  a)  = fold' p palg alg a
  fold' p palg (_, alg) (R  b)  = fold' p palg alg b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |Fold'|}

The fall-back |K| instance:
\begin{code}
instance Fold' (K a) t where
  fold' p _ alg (K a) = alg (Left a)
\end{code}

PAUSE_LINE

The recursive |K| instance:
\begin{code}
instance Fold t => Fold' (K (PURPLE(t))) (PURPLE(t)) where
  fold' p palg alg (K t) = (PURPLE(alg)) (Right (fold palg t))
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |Fold'|}

The fall-back |:*:| instance:
\begin{code}
instance Fold' b t => Fold' (K a :*: b) t where
  fold' p palg alg (K a :*: b) = fold' p palg (alg (Left a)) b
\end{code}

PAUSE_LINE

The recursive |:*:| instance:
\begin{code}
instance (Fold t, Fold' b t) => Fold' (K (PURPLE(t)) :*: b) (PURPLE(t)) where
  fold' p palg alg (K a :*: b) = fold' p palg ((PURPLE(alg)) (Right (fold palg a))) b
\end{code}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining |fold|}

The improved |Fold| class:
\begin{code}
class Fold a where
  fold :: Alg (Rep a) r -> a -> r

  default fold :: (Generic a, Fold' (Rep a) a) => Alg (Rep a) r -> a -> r
  fold alg x = fold' (Just x) alg alg (from x)
\end{code}
\begin{itemize}INCREMENT

\item We use |Maybe| as a simple proxy.

\item The algebra is needed twice: the second argument is pattern-matched by
the instances of |Fold'|.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Fold and Beyond}

We presented a generic recursive pattern in a library that would not typically
have it.

\begin{itemize}INCREMENT

\item We can also define many other (co-)recursive functions, including the
generic zipper.

\item The unfortunate aspect of |Alg| is that we must use |Either| since, in
the type family, we cannot distinguish overlapping instances.

\item We believe this can be fixed with the new ordered overlapping instances
in GHC.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Conclusions}

\begin{itemize}INCREMENT

\item We believe generic programming is easy to understand if looked at from
the right perspective.

\item We are still searching for that optimal view.

\item The library presented here is quite simple.

\item Yet, with a few tricks, it is also quite powerful.

\item We have also done this work in the more complicated Generic Deriving
library.

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{References}

\begin{itemize}

\item Johan Jeuring, Sean Leather, Jos\'{e} Pedro Magalh\~{a}es, Alexey
Rodriguez Yakushev. \textit{Libraries for Generic Programming in Haskell}. AFP
2008. pp. 165-229, 2009.

\item Generic Deriving: \url{http://www.haskell.org/haskellwiki/GHC.Generics}

\item Generic Deriving Extras: \url{https://github.com/spl/generic-deriving-extras}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------

\end{document}


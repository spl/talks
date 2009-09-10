
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
\newcommand{\pkg}[1]{\textit{#1}}

\newcommand{\bs}{%
\setlength\belowdisplayskip{-10pt}}

%-------------------------------------------------------------------------------
% Titles

%-------------------------------------------------------------------------------

\begin{document}

%-------------------------------------------------------------------------------

%if style == newcode
\begin{code}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module Talk where

newtype Id a = Id a

instance Functor Id where
  fmap f (Id x) = Id (f x)

newtype Arr a b = Arr (a -> b)

instance Functor (Arr a) where
  fmap f (Arr g) = Arr (f . g)

newtype (:.:) f g a = Comp (f (g a))

infixr 8 :.:

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp fga) = Comp (fmap (fmap f) fga)

(<>) :: (Functor f, Functor g) => f (b -> c) -> g (a -> b) -> (:.:) f g (a -> c)
f <> g = Comp (fmap (\s -> fmap (\t -> s . t) g) f)
infixr 8 <>

class (Functor (F d)) => Format d where
  type F d :: * -> *
  showsf' :: d -> F d ShowS

instance Format String where
  type F String = Id
  showsf' s = Id (showString s)

data StringF = String

instance Format StringF where
  type F StringF = Arr String
  showsf' String = Arr showString

data NumF a = Num

instance (Num a) => Format (NumF a)  where
  type F (NumF a) = Arr a
  showsf' Num = Arr shows

data a :%: b = a :%: b
infixr 8 :%:

(%) :: a -> b -> a :%: b
(%) = (:%:)
infixr 8 %

instance (Format d1, Format d2) => Format (d1 :%: d2)  where
  type F (d1 :%: d2) = F d1 :.: F d2
  showsf' (d1 :%: d2) = showsf' d1 <> showsf' d2

---

instance Apply (Arr a) b where
  type R (Arr a) b = a -> b
  apply (Arr f) = f

instance Apply Id a where
  type R Id a = a
  apply (Id a) = a

instance (Apply f (R g a), Apply g a) => Apply (f :.: g) a where
  type R (f :.: g) a = R f (R g a)
  apply (Comp fga) = apply (fmap apply fga)

\end{code}
%endif

%-------------------------------------------------------------------------------

\title{Extensibility and type safety in formatting}
\subtitle{The design of \pkg{xformat}}

\author{Sean Leather}

\date{11 September 2009}

%-------------------------------------------------------------------------------
\begin{frame}

\titlepage

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{The problem with |printf|}

Given
\begin{spec}
int printf(const char *format, ...)
\end{spec}
we can write
{\small
\begin{verbatim}
> printf("%s W%drld!\n", "Hello", 0);
Hello W0rld!
\end{verbatim}
}
\onslide<2->
But we can also write
{\small
\begin{verbatim}
> printf("%s W%drld!\n", 0, "Hello");
(null) W134514152rld!
\end{verbatim}
}
unintentionally, of course.
\onslide<3->
Or even
{\small
\begin{verbatim}
> printf("%s W%drld!\n", "Hello");
Hello W134514152rld!
\end{verbatim}
}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{The problem with |scanf|}

Given
\begin{spec}
int scanf (char *format, ...)
\end{spec}
we have the same problems as we do with |printf|.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Simple functions with big problems}

The functions |printf| and |scanf| are extremely handy, but they should be
considered ``unsafe.''

\onslide<2->
The problems:
\begin{itemize}

\onslide<3->
\item No validation of the argument types
\begin{itemize}
\item Which argument is a string or an integer?
\end{itemize}

\onslide<4->
\item No validation of the arity
\begin{itemize}
\item How many arguments does the format spec call for?
\end{itemize}

\onslide<5->
\item Unchangeable format specification
\begin{itemize}
\item We're stuck with |%s|, |%d|, etc.
\item Character specs are left over from a time when the types were few and the
operations expected.
\end{itemize}

\end{itemize}

\onslide<6->
In C, |printf| can result in buffer overflows among other problems.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{|Text.Printf| to the rescue?}

In the \pkg{base} package, we have a solution: |Text.Printf|. Given

\begin{spec}
printf :: (PrintfType rr) => String -> rr
\end{spec}
we can write
{\small
\begin{verbatim}
> printf "%s W%drld!\n" "Hello" 0
Hello W0rld!
\end{verbatim}
}
\onslide<2->
But we can also write
{\small
\begin{verbatim}
> printf "%s W%drld!\n" 0 "Hello"
Stopped at <exception thrown>
\end{verbatim}
}
\onslide<3->
Or even
{\small
\begin{verbatim}
> printf "%s W%drld!\n" "Hello"
Hello WStopped at <exception thrown>
\end{verbatim}
}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{|Text.Printf|: Savior Fail}

Oops!

\onslide<2->
Using |Text.Printf| doesn't overflow the buffer, but it doesn't fix any of the
problems with |printf| in C.

\begin{itemize}
\item No validation of the argument types
\item No validation of the arity
\item Unchangeable format specification
\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\pkg{xformat}: save us!}

In Haskell, we like strongly typed functions, and we (often) like our functions
to be total. Enter \pkg{xformat}:
\begin{spec}
showf :: (Format dd ff, Apply ff String aa) => dd -> aa

readf :: (Format dd aa) => dd -> String -> Maybe aa
\end{spec}
Now, we can write
{\small
\begin{verbatim}
> putStr $ showf (String % " W" % Num % "rld!\n") "Hello" 0
Hello W0rld!
\end{verbatim}
}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\pkg{xformat}: type-save us!}

But the typechecker prevents us from writing
{\small
\begin{verbatim}
> putStr $ showf (String % " W" % Num % "rld!\n") 0 "Hello" 
<interactive>:1:9:
    No instance for (Num [Char])
    ...
\end{verbatim}
}
\onslide<2->
or
{\small
\begin{verbatim}
> putStr $ showf (String % " W" % Num % "rld!\n") "Hello" 
    Couldn't match expected type `[Char]'
           against inferred type `a -> [Char]'
      Expected type: String
      Inferred type: a -> [Char]
      ...
\end{verbatim}
}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{\pkg{xformat}: solver of all problems?}

Does \pkg{xformat} handle our problems?

\begin{itemize}

\onslide<2->
\item No validation of the argument types
\begin{itemize}
\item Yes, the typechecker does this.
\end{itemize}

\onslide<3->
\item No validation of the arity
\begin{itemize}
\item Yes, the typechecker does this.
\end{itemize}

\onslide<4->
\item Unchangeable format specification
\begin{itemize}
\item Yes, thanks to a generic programming technique.
\end{itemize}

\end{itemize}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{The core of |Text.XFormat.Show|}

This is the class used to define all format descriptors |dd|.
\begin{spec}
class (Functor ff) => Format dd ff | dd -> ff where
  showsf' :: dd -> ff ShowS
\end{spec}
Recall:
\begin{spec}
type ShowS = String -> String
\end{spec}
Each format descriptor results in a value of one of the following functors.
\begin{spec}
newtype Id aa           = Idd aa             deriving Functor

newtype Arr aa bb       = Arrr (aa -> bb)    deriving Functor

newtype (:.:) ff gg aa  = Comp (ff (gg aa))  deriving Functor
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining descriptors (1)}

Descriptors are actually quite easy to define.

\onslide<2->
For a format constant (e.g. a string), we use the type of that constant (e.g.
|String|) as the descriptor.
\begin{spec}
instance Format String Id where
  showsf' s = Id (showString s)
\end{spec}
\onslide<3->
For a format placeholder, we create a unit type. The constructor serves as the
descriptor, and the type serves as a reference to the type of the format.
\begin{spec}
data StringF = String'

instance Format StringF (Arr String) where
  showsf' String' = Arr showString
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Defining descriptors (2)}

We can make many more interesting descriptors.

\onslide<2->
Type class dependencies:
\begin{spec}
data NumF aa = Num

instance (Num aa) => Format (NumF aa) (Arr aa) where
  showsf' Num = Arr shows
\end{spec}
\onslide<3->
Recursive formats for containment and composition:
\begin{spec}
data a :%: b = a ::%:: b

instance  (Format dd1 ff1, Format dd2 ff2)
          => Format (dd1 :%: dd2) (ff1 :.: ff2) where
  showsf' (d1 ::%:: d2) = showsf' d1 <> showsf' d2

f <> g = Comp (fmap (\s -> fmap (\t -> s . t) g) f)
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Where are we?}

But what do all these descriptors do for us?

\onslide<2->
Given the format we used before
{\small
\begin{verbatim}
> :t showsf' (String % " W" % Num % "rld!\n")
\end{verbatim}
}
\onslide<3->
we learn that its type is
\begin{spec}
(Num aa) =>  (Arr String :.: Id :.: Arr aa :.: Id) ShowS
\end{spec}
So we now see how the functor |newtype|s play a role. But we don't want to type
those as arguments to the function.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Resolving functors (1)}

We resolve them using this class
\begin{spec}
class (Functor ff) => Apply ff aa bb | ff aa -> bb where
  apply :: ff aa -> bb
\end{spec}
\onslide<2->
The instances indicate the functional dependency from the functor to its
resolved type.
\begin{spec}
instance Apply Id aa aa where
  apply (Idd a) = a

instance Apply (Arr aa) bb (aa -> bb) where
  apply (Arrr f) = f

instance (Apply ff bb cc, Apply gg aa bb) => Apply (ff :.: gg) aa cc where
  apply (Comp fga) = apply (fmap apply fga)
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Resolving functors (2)}

To see this in action, we can look at the type again
{\small
\begin{verbatim}
> :t apply $ showsf' (String % " W" % Num % "rld!\n")
\end{verbatim}
}
\onslide<2->
to learn that it is
\begin{spec}
(Num aa) => String -> aa -> String -> String
\end{spec}
\onslide<3->
And finally we can get to |showf| that we saw earlier
\begin{spec}
showf :: (Format dd ff, Apply ff String aa) => dd -> aa
showf d = apply (fmap ($ "") (showsf' d))
\end{spec}

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Last note: functional dependencies vs. associated types}

I used functional dependencies for \pkg{xformat} after trying both approaches.
Suppose I defined the class |Apply| as
\begin{code}
class (Functor ff) => Apply ff aa where
  type R ff aa :: *
  apply :: ff aa -> R ff aa
\end{code}
\onslide<2->
Then the following type inference
{\small
\begin{verbatim}
> :t apply $ showsf' (String % " W" % Num % "rld!\n")
\end{verbatim}
}
\onslide<3->
gives me
\begin{spec}
(Num aa) => R (Arr String :.: Id :.: Arr aa :.: Id) (String -> String)
\end{spec}
It's not incorrect, but it's not the resolved type I'm looking for.

\end{frame}
%-------------------------------------------------------------------------------
\begin{frame}
\frametitle{Conclusions and Future}

We looked at a recently release package called \pkg{xformat} with extensible,
type-safe |printf|- and |scanf|-like functions. Though we didn't cover
|Text.XFormat.Read| and |readf|, the design is similar and simpler.

\onslide<2->
The research behind this can be found in articles by Olivier Danvy and Ralf
Hinze linked at \url{http://www.citeulike.org/user/spl/tag/printf}.

\onslide<3->
After talking to Bryan O'Sullivan about this, I'm thinking about developing a
quasiquoter on top of \pkg{xformat} so you can write your format descriptor
something like
{\small
\begin{verbatim}
> putStr $ showf [fmt|"{0} W{1}rld!\n"|] "Hello" 0
\end{verbatim}
}
The descriptor is no longer extensible, but apparently some people care more
about conciseness.

\end{frame}
%-------------------------------------------------------------------------------
\end{document}


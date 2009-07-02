
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

\newcounter{countA}
\newcommand{\EqualityExampleTitle}{\CountingTitle{Defining an Example: Equality}{countA}}

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

\begin{spec}
data Tree = Tip | Leaf Int | Bin Int Tree Tree
\end{spec}

There are multiple \b{generic views} of the structure. SYB uses one based on
combinators. EMGM uses a different one based on sums of products.

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{Representing Structure in EMGM}

To view the |Tree| type in its structure representation...

\begin{code}
data Tree = Tip | Leaf Int | Bin Int Tree Tree
\end{code}

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

\begin{code}
data UnitT = Unit
\end{code}
%if style == newcode
\begin{code}
  deriving (Enum, Eq, Ord, Read, Show)
\end{code}
%endif

\begin{code}
data aa :+: bb = L aa | R bb
\end{code}
%if style == newcode
\begin{code}
  deriving (Eq, Ord, Read, Show)
\end{code}
%endif

\begin{code}
data aa :*: bb = aa ::*:: bb
\end{code}
%if style == newcode
\begin{code}
  deriving (Eq, Ord, Read, Show)
\end{code}
%endif

\begin{code}
data EPT dd rr = EP { from :: (dd -> rr), to :: (rr -> dd) }
\end{code}

\begin{code}
data ConDescrT   = ConDescr dots
\end{code}
%if style == newcode
\begin{code}
  deriving (Enum, Eq, Ord, Read, Show)
\end{code}
%endif
\begin{code}
data TypeDescrT  = TypeDescr dots
\end{code}
%if style == newcode
\begin{code}
  deriving (Enum, Eq, Ord, Read, Show)
\end{code}
%endif

\end{frame}

%-------------------------------------------------------------------------------

\begin{frame}

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
\end{code}
%endif

\end{frame}

%-------------------------------------------------------------------------------

\end{document}


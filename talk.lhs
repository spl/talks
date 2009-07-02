
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

\title{\textbf{Fun} and \textbf{generic} things to do with \textbf{EMGM}}

\author{Sean Leather}

\date{9 July 2009}

%-------------------------------------------------------------------------------

\frame{ \titlepage }

%-------------------------------------------------------------------------------

\begin{frame}

\frametitle{Overview}

\begin{itemize}

\item (Datatype-)Generic Programming

\item Extensible and Modular Generics for the Masses

\begin{itemize}
\item History
\item What the Package Provides
\end{itemize}

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


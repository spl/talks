
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

\end{document}


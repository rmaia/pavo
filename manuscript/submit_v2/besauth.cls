%---------------------------------------------------------------------------
%Please  be  aware that the use of this LaTeX class file is governed by the
%following conditions:
%
% based on the original LaTeX ARTICLE DOCUMENT STYLE
% Copyright (C) 1988, 1989 by Leslie Lamport
%
% Copyright (c) 2011 John Wiley & Sons, Ltd, The Atrium, Southern Gate, Chichester,
% West Sussex, PO19 8SQ UK.   All rights reserved.
%
%Rules of Use
%
%% You are NOT ALLOWED to change this file.
%
%
%This class file is made available for use by authors who wish to prepare an
%article for publication in
%a journal of the British Ecological Society
%published by John Wiley & Sons Ltd. The user may not exploit any part of
%the class file commercially.
%
%This class file is provided on an `as is'  basis, without warranties of any
%kind, either expressed or implied, including but not limited to warranties of
%title, or  implied warranties of merchantablility or fitness for  a
%particular purpose. There will be no duty on the author[s] of the software
%or John Wiley & Sons Ltd to correct any errors or defects in the software.
%Any statutory rights you may have remain unaffected by your acceptance of
%these rules of use.
%---------------------------------------------------------------------------
%
% Created by Alistair Smith, Sunrise Setting Ltd, 8th March 2011
%
% besauth.cls --- For the British Ecological Society

\def\update{2011/04/21 v1.10}

\newcommand{\journalclass}{besauth.cls}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{bcs}[\update\ \journalclass]

%\newcommand\hmmax{0}

\newif\if@doublespace
\DeclareOption{doublespace}{%
\@doublespacetrue}

\newif\if@timesfont
\DeclareOption{times}{%
\@timesfonttrue}

\newif\if@jea
\DeclareOption{jea}{%
\@jeatrue}

\newif\if@fec
\DeclareOption{fec}{%
\@fectrue}

\newif\if@jec
\DeclareOption{jec}{%
\@jectrue}

\newif\if@jpe
\DeclareOption{jpe}{%
\@jpetrue}

\newif\if@mee
\DeclareOption{mee}{%
\@meetrue}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}
\if@timesfont
\RequirePackage{times}
\fi
\if@doublespace
\RequirePackage[onehalfspacing]{setspace}
\fi
\if@jea
\newcommand{\journalname}{Journal of Animal Ecology}
\fi
\if@fec
\newcommand{\journalname}{Functional Ecology}
\fi
\if@jec
\newcommand{\journalname}{Journal of Ecology}
\fi
\if@jpe
\newcommand{\journalname}{Journal of Applied Ecology}
\fi
\if@mee
\newcommand{\journalname}{Methods in Ecology \& Evolution}
\fi

\RequirePackage{graphicx}
\RequirePackage{pifont,latexsym,ifthen,rotating,calc,textcase,booktabs}
\RequirePackage[fleqn]{amsmath}
\RequirePackage{amsfonts,amssymb,amsbsy,amsthm}
\RequirePackage{titlesec,enumitem}
%\RequirePackage{bm}
\RequirePackage[errorshow]{tracefnt}

\def\tagform@#1{\maketag@@@{eqn \ignorespaces#1\unskip}}
\@mathmargin = 0pt

\@twosidetrue
\flushbottom
\frenchspacing

\textwidth 167mm
\textheight 239mm
\setlength\columnsep{7mm}

%Trim sizes
\setlength\voffset{-1in}
\setlength\hoffset{-1in}
\topmargin 10mm
\setlength\oddsidemargin{21.5mm}%back margin on odd pages
\setlength\evensidemargin{21.5mm}%fore margin on even pages
\setlength\paperwidth{210mm}
\setlength\paperheight{297mm}
%Needed to set PDF page size
\special{papersize=210mm,297mm}

\parskip \z@
\parindent 1em
\headheight 50pt
\headsep    15pt 
\footskip 18pt

\hyphenpenalty=1000
\pretolerance=8000
\tolerance=9500
\hbadness=8000
\vbadness=9000
\displaywidowpenalty=0
\clubpenalty=10000
\widowpenalty=10000
\lefthyphenmin=3
\righthyphenmin=3
\brokenpenalty=10000
\hbadness=10000
\vbadness=10000

\thinmuskip = 3mu
\medmuskip = 4mu
\thickmuskip = 5mu

\setcounter{topnumber}{10}
\def\topfraction{1}
\setcounter{bottomnumber}{10}
\def\bottomfraction{0.8}
\setcounter{totalnumber}{10}
\def\textfraction{0}
\renewcommand{\floatpagefraction}{0.95}
\setcounter{dbltopnumber}{10}
\renewcommand{\dblfloatpagefraction}{0.95}
\renewcommand{\dbltopfraction}{1}

\renewcommand\normalsize{%
   \@setfontsize\normalsize\@xpt\@xiipt
   \abovedisplayskip 10\p@ \@plus2\p@ \@minus5\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
   \belowdisplayskip \abovedisplayskip
   \let\@listi\@listI}
%\normalsize
\renewcommand\small{%
   %\@setfontsize\small\@ixpt{11}%
   \@setfontsize\small\@ixpt{12}%
   \abovedisplayskip 8.5\p@ \@plus3\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 4\p@ \@plus2\p@ \@minus2\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\small
\renewcommand\footnotesize{%
   %\@setfontsize\footnotesize\@viiipt{9.5}%
   \@setfontsize\footnotesize\@viiipt{10}%
   \abovedisplayskip 6\p@ \@plus2\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus\p@
   \belowdisplayshortskip 3\p@ \@plus\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 3\p@ \@plus\p@ \@minus\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\renewcommand\scriptsize{\@setfontsize\scriptsize\@viipt\@viiipt}
\renewcommand\tiny{\@setfontsize\tiny\@vpt\@vipt}
\renewcommand\large{\@setfontsize\large\@xiipt{14}}
\renewcommand\Large{\@setfontsize\Large\@xivpt{18}}
\renewcommand\LARGE{\@setfontsize\LARGE\@xviipt{20}}
\renewcommand\huge{\@setfontsize\huge\@xxpt{23}}
\renewcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}

\newcommand{\titlesize}{\fontsize{18pt}{22pt}\selectfont}
\newcommand{\authorsize}{\fontsize{11pt}{15pt}\selectfont}

\def\copyrightline#1{%
\gdef\@copyrightline{{#1}}}
\let\@copyrightline\@empty

\gdef\@copyrightline{\copyright\ \volumeyear\ The Authors. Journal compilation \copyright\ \volumeyear\ British Ecological Society}

\gdef\@doiline{{doi:}\ 10.1111/\dots}

\newbox\absbox
\def\abstract{\lrbox\absbox\minipage{144mm}%
  \normalfont%
  {\section{Summary}}\normalsize%
  }
\def\endabstract{\endminipage\endlrbox}

\def\keywords#1{%
  \gdef\@keywords{\begin{minipage}{144mm}{\sf\bfseries Key-words:}\ {#1}\end{minipage}}}
\let\@keywords\@empty

\skip\footins 22pt plus  8pt
\gdef\footnoterule{}
%\def\footnoterule{\kern-3\p@
%  \hrule \@width \columnwidth \kern 2.6\p@}

\renewcommand{\thefootnote}{\fnsymbol{footnote}}
\long\def\@makefntext#1{\parindent 1em%
\noindent{$\m@th^{\@thefnmark}$}#1}

%\footnotesep 10pt

\def\corraddr#1{%
  \gdef\@corraddr{%
   {\footnotetext[0]{$^{\text{*}}$Correspondence author. E-mail: #1}}}}
\let\@corraddr\@empty
\def\corrauth{$^{\text{*}}$}

\def\address#1{%
  \gdef\@address{{#1}}}
\let\@address\@empty

\def\papertype#1{%
  \gdef\@papertype{{#1}}}
\let\@papertype\@empty

\def\cgsn#1#2{%
\gdef\@cgsn{%
\footnotetext[0]{%
Contract/grant sponsor: #1; contract/grant
number: #2}}}

\def\cgs#1{%
\gdef\@cgs{%
\footnotetext[0]{%
Contract/grant sponsor: #1}}}

\let\@cgsn\@empty
\let\@cgs\@empty

\def\affilnum#1{${}^\text{\sf\bfseries #1}$}
\def\affil#1{${}^\text{\sf #1}$}
%\def\comma{${}^{\text{,}}$}

\renewcommand\maketitle{\par
  \begingroup
     \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
       \fi
    \else
      \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \thispagestyle{title}\label{FirstPage}\@corraddr\@cgs\@cgsn
  \endgroup
  %\setcounter{footnote}{0}%
  \global\let\address\relax
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\@address\@empty
  \global\let\corraddr\relax
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
\small}
\def\@maketitle{%
\null%
{\raggedright\large\sf\bfseries\MakeUppercase{\@papertype} \par}%
  {\raggedright\titlesize\sf\bfseries\@title \par}%
    \vskip 14pt %
    {\raggedright\authorsize\sf\bfseries\@author\par}%
    \vskip 9pt%
    {\small\raggedright\sf\itshape\@address\par}
    \vskip 9pt
    \rule{\textwidth}{0.75pt}%
    \vskip 21pt%
    {\centerline{\usebox\absbox}\par}
    \vskip 10pt%
    {\centerline{\@keywords}\par}
    \vspace{14pt}%
    }
 
\def\startpage{\pageref{FirstPage}}
\def\endpage{\pageref{LastPage}}
\def\volumeyear{0000}

\def\runningheads#1#2{\markboth{{#1}}{{#2}}}

\gdef\titlepagefooter{%
\parbox[b]{\textwidth}{\footnotesize%
\begin{tabular}[t]{@{}l@{}}
\@copyrightline\\
\textit{Prepared using \textsf{\journalclass} [Version: \update]}
\end{tabular}%
}}

\def\ps@title{%
\def\@oddhead{%
\parbox[b]{\textwidth}{\small\textit{\journalname}\ \volumeyear\hfill\@doiline}}%
\let\@evenhead\@oddhead
\def\@oddfoot{%
\titlepagefooter%
}
\let\@evenfoot\@oddfoot}

\def\ps@bespage{%
\let\@mkboth\@gobbletwo
\def\@evenhead{\parbox[b]{\textwidth}{%
\small\textbf{\thepage}\hspace{1em}\textit{\leftmark}\hfill\mbox{}}}
\def\@oddhead{\parbox[b]{\textwidth}{%
\mbox{}\hfill\small\textit{\rightmark}\hspace{1em}\textbf{\thepage}}}
\def\@evenfoot{\parbox[t]{\textwidth}{\footnotesize%
\mbox{}\hfill\@copyrightline, \textit{\journalname}\\
\mbox{}\hfill\emph{Prepared using \textsf{\journalclass}}}}
\def\@oddfoot{\parbox[t]{\textwidth}{\footnotesize%
\@copyrightline, \textit{\journalname}\hfill\mbox{}\\
\emph{Prepared using \textsf{\journalclass}}\hfill\mbox{}}}%
}

\setcounter{secnumdepth}{0}
\titlelabel{\thetitle.}

%\newcommand*\sectitle[1]{%
%\normalfont\large\sf\bfseries\raggedright}

\titleformat{\section}[block]
{\normalfont\normalsize\sf\bfseries}
{\thesection.}
{0.5em}
{\normalfont\normalsize\sf\bfseries\raggedright}

\titlespacing*{\section}{0pt}{10pt \@plus 2pt \@minus 2pt}{6pt\@plus 2pt \@minus 2pt}


\newcommand*\subsectitle[1]{%
\raggedright\sf\footnotesize\MakeUppercase{#1}}

\titleformat{\subsection}[block]
{\normalfont\normalsize\sf}
{\thesubsection.}
{0.5em}
{\subsectitle}

\titlespacing*{\subsection}{0pt}{10pt \@plus 2pt \@minus 2pt}{6pt\@plus 2pt \@minus 2pt}

\titleformat{\subsubsection}[block]
{\normalfont\small\sf\itshape}
{\thesubsubsection.}
{0.5em}
{\normalfont\small\sf\itshape\raggedright}

\titlespacing*{\subsubsection}{0pt}{10pt \@plus 2pt \@minus 2pt}{6pt\@plus 2pt \@minus 2pt}

\setlist{partopsep=0pt,topsep=0pt}

%\renewcommand{\thetable}{\Roman{table}}
\renewcommand{\figurename}{Fig.}

\usepackage{caption}
\DeclareCaptionLabelSeparator{wileyperiod}{.\hspace*{0.5em}}
\captionsetup[figure]{font=footnotesize,labelfont=bf,labelsep=wileyperiod,justification=justified,singlelinecheck=true}
\captionsetup[table]{position=top,font=footnotesize,labelfont=bf,labelsep=wileyperiod,justification=justified,singlelinecheck=true}

\def\thmhead@plain#1#2#3{%
  \thmname{#1}\thmnumber{\@ifnotempty{#1}{ }{#2}}%
  \thmnote{ {\the\thm@notefont(#3)}}}

\newenvironment{mandm}{%
\section{Materials and methods}\par%
\footnotesize\selectfont}%
{\par\small\selectfont}


\newtheoremstyle{wiley}
{6pt plus 2pt minus 2pt}% space above
{6pt plus 2pt minus 2pt}% space below
{}% Body font
{}% Indent amount
{\itshape}% Theorem head font
{}% Punctuation after theorem head
{\newline}% Space after theorem head
{}% Theorem head spec

\renewenvironment{proof}[1][\proofname]{\par
  \pushQED{\qed}%
  \normalfont \topsep6\p@\@plus6\p@\relax
  \trivlist
  \item[\hskip\labelsep
        \itshape
    #1\@addpunct{}]\mbox{}\newline\ignorespaces
}{%
  \popQED\endtrivlist\@endpefalse
}

\theoremstyle{wiley}

\newcommand\ack{\section*{Acknowledgement}\scriptsize}
\newcommand\acks{\section*{Acknowledgements}\scriptsize}

\renewcommand\refname{References}
\usepackage{natbib}
\setlength{\bibsep}{0pt}
\setlength{\bibhang}{0.75em}
\renewcommand{\bibfont}{\scriptsize}

%\renewenvironment{thebibliography}[1]{%
%\section*{\refname}
%\list{{\arabic{enumi}}}{\def\makelabel##1{\hss{[##1]}}\topsep=0\p@\parsep=0\p@
%\partopsep=0\p@\itemsep=3\p@
%\labelsep=1ex\itemindent=0\p@
%\settowidth\labelwidth{\small[#1]}%
%\leftmargin\labelwidth
%\advance\leftmargin\labelsep
%\advance\leftmargin -\itemindent
%\usecounter{enumi}}\small
%\def\newblock{\ }
%\sloppy\clubpenalty4000\widowpenalty4000
%\sfcode`\.=1000\relax}{\endlist}

%\def\biog{\section*{Author's Biography}\small}
%\def\biogs{\section*{Authors' Biographies}\small}

\AtEndDocument{%
\label{LastPage}}

\hyphenation{com-mu-ni-ca-tions}

\pagestyle{bespage}
%\twocolumn
\small
\sloppy

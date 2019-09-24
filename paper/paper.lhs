\documentclass[runningheads]{llncs}

\input{sections/01-packages.tex}

\title{Effectful DSL for UI Animations}

\input{sections/02-commands.tex}

\ifx\tocompile
\input{sections/03-code.tex}
\else
\fi

\begin{document}

\author{Ruben~P. Pieters $^{\textrm{\Letter}}$ \orcidID{0000-0003-0537-9403} \and
Tom Schrijvers \orcidID{0000-0001-8771-5559}}

\authorrunning{R. P. Pieters and T. Schrijvers}

\institute{KU Leuven, 3001 Leuven, Belgium
\\\email{\{ruben.pieters, tom.schrijvers\}@@cs.kuleuven.be}}

\maketitle

\input{sections/04-abstract.tex}

\input{sections/05-intro.tex}

\input{sections/06-motivation.tex}

\include{paper_bib}

\end{document}

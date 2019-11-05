\documentclass[runningheads]{llncs}

\input{sections/01-packages.tex}

\title{PaSe: An Extensible and Inspectable DSL\\for Micro-Animations}

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

\input{sections/07-features.tex}

\input{sections/08-application.tex}

\input{sections/09-detail.tex}

\input{sections/10-interaction.tex}

\input{sections/11-evaluation.tex}

\input{sections/97-future.tex}

\input{sections/98-related.tex}

\input{sections/99-conclusion.tex}

\input{paper_bib}

\end{document}

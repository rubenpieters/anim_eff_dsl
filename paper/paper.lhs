\documentclass[runningheads]{llncs}

\input{sections/01-packages.tex}

\title{PaSe: An Extensible and Inspectable DSL\\for Micro-Animations}}

\input{sections/02-commands.tex}

% for review
\usepackage{lineno}
\usepackage{mdframed}
\newmdtheoremenv[linewidth=0.5pt, topline=false, bottomline=false, rightline=false,%
leftmargin=0pt, innerleftmargin=0.4em, rightmargin=0pt, innerrightmargin=0pt, innertopmargin=-5pt ,%
innerbottommargin=3pt, splittopskip=\topskip, splitbottomskip=0.3\topskip, %
skipabove=0.6\topsep]%
{quote}{}%

\begin{document}

% for review
\linenumbers

\author{Ruben~P. Pieters $^{\textrm{\Letter}}$ \orcidID{0000-0003-0537-9403} \and
Tom Schrijvers \orcidID{0000-0001-8771-5559}}

\authorrunning{R. P. Pieters and T. Schrijvers}

\institute{KU Leuven, 3001 Leuven, Belgium
\\\email{\{ruben.pieters, tom.schrijvers\}@@cs.kuleuven.be}}

\maketitle
\\First author is a \emph{research student}. Article category: \emph{Project Article}.

\input{sections/04-abstract.tex}

\input{sections/05-intro.tex}

\input{sections/06-motivation.tex}

\input{sections/07-features.tex}

\input{sections/09-detail.tex}

\input{sections/10-interaction.tex}

\input{sections/11-evaluation.tex}

\input{sections/12-evaluation2.tex}

\input{sections/97-future.tex}

\input{sections/98-related.tex}

\input{sections/99-conclusion.tex}

\input{paper_bib}

% for review
\appendix

\section{Response to Reviewer 1}

\begin{quote}
This paper describes the design and implementation of an EDSL for combining animations. The basic underlying technique used is the taggless final style, which offers good extensibility. The paper makes special effort in discussing the inspecablity property and the implication of that on various program features, particularly the monadic bind operator.
\\
\\I think this is a good paper suitable for publication at TFP. The paper is relatively lightweight, but very neat. Though the underlying embedding technique is not new, it is used very appropriately for the application context, and resulting language is quite attractive. The presentation of the paper is very good. The implementation is explained clearly at the right level for TFP audience. My only comment is that the first paragraph of the introduction is slightly misleading. Its criticism of monad is certainly valid, but out of place.
\end{quote}

We have reworked the introduction. It is now more focused on the trade-off between expressivity and inspectability, and its revelance to PaSe.

\section{Response to Reviewer 2}

\begin{quote}
Overall, my feeling with this paper is that this is a good start on the design but the evaluation of the design is lacking. There does not seem to be any interesting application built using this library that could give the authors some good feedback into the design itself. There are some pictures shown in the beginning of the paper, but there isn't any discussion of connection to a web browser or other GUI system so I am not even clear if these pictures are mockups or actually captured by running code from the library.
\end{quote}

The application discussed in the paper uses the gloss library as its graphics library. In the meantime we have also developed some other applications (another demo, an communication story example and a pacman game), some of which directly use the Haskell SDL bindings. We now emphasize these implementations more in the paper.

\begin{quote}
It is a shame that the inspectability design constraint means that existing control operators cannot be used anymore.
\end{quote}

The inspectability feature is opt-in, this means that it is perfectly fine to use traditional Monad constraints on animations if inspectability is no concern. Another way to regain the existing if-then-else for animations with inspectability requirement is the use of GHC's RebindableSyntax extension. We have added these notes to the paper.

\begin{quote}
One (perhaps naive) thing I wonder: why not simply run the animation and see how long it was (perhaps caching the result)? I understand that won't work if, say, the duration depends on some external state (like the specific bytes coming over some network or something) but it seems like an approach like that would at least let you re-use the existing Haskell conditional operations and would probably give you the answer for any interesting animation, in practice.
\end{quote}

Simulating a pure animation (one without any external effects) is certainly possible. However, when you simulate a conditional animation you will simulate one particular branch. The two animations in these branches could have different durations, and thus the result of this simulation can give you a wrong result if that is not the actual branch taken. This is where the inspectability aspect comes into play: we are statically 'evaluating' the duration of the animation and taking every possible branch into account. Hence we obtain a minimum or maximum duration, rather than just a duration. The ability to handle impure animations is also a requirement, since in our experience it is very useful to have the expressivity of embedding new effects within animations.

We have added a paragraph in section 4.4 containing these thoughts.

\begin{quote}
For those readers not familiar with the various details of thing like "the Functor-Applicative-Monad hierarchy", sections 4.1 and 4.2 were difficult to read. The `Animation` type and the related discussion in 4.3 was very helpful to understand how animations really work.
\end{quote}

The \emph{sequential composition} paragraph of section 4.2 has been extended.

\begin{quote}
Section 4.4, again, was difficult; I do not know what "interpreting PaSe to a different data type" means and I couldn't figure out what was really going on.
\end{quote}

Reworked the introductory part of section 4.4.

\begin{quote}
In 6.3, I didn't understand why the duration was important. In particular, it would seem that if one wants to sequence animations, one could simply wait until the first animation finished (however long that was) and not need to know up front how long the first animation was going to be.
\end{quote}

The feature in 6.3 is not ordinary sequencing, but relative sequencing. Relative sequencing differs as it is possible to state that an animation must start *before* the end time of another animation. This means that it is not possible to wait until the first animation is finished, as the sequenced animation must already have started at that point.

We have added a paragraph in section 6.3 explaining this.

\begin{quote}
pg8: "opt foran" =$>$ "opt for an"
\end{quote}

Fixed.

\begin{quote}
Overall, my advice to the authors is to spend some time implementing something with the library that attempts to captures something useful (possibly something useful in your own life, separatly from your research work even) and then try to use it to get some feedback on the design. Report on this application and see how it influences the design. Does the inspectability issue matter in practice? (Why?) Does something else come up that needs some thought or care?
\end{quote}

We have since implemented a demo application, a communication story example and a Pac-Man game. We have not encountered any noteworthy difficulties with respect to creating animations. In contrast, with GSAP, we did encounter a difficulty when implementing a general function for particle animations. We also recreated a GSAP benchmark of parallel animations, where PaSe scores slightly worse. We were able to utilize inspectability to automatically fetch the used textures in the animations. We have added an evaluation section in the paper detailing this use case evaluation.

\section{Response to Reviewer 3}

\begin{quote}
The paper introduces PaSe -- a DSL embedded in Haskell for expressing micro-animations. In particular the authors target two aspects: extensibility and inspectability. The paper is very well written and provides interesting insights into different aspects of PaSe. The only limitation of the paper is a lack of any kind of evaluation to demonstrate effectiveness and performance of the approach.
\end{quote}

We have since implemented a demo application, a communication story example and a Pac-Man game. We have not encountered any noteworthy difficulties with respect to creating animations. In contrast, with GSAP, we did encounter a difficulty when implementing a general function for particle animations. We also recreated a GSAP benchmark of parallel animations, where PaSe scores slightly worse. We were able to utilize inspectability to automatically fetch the used textures in the animations. We have added an evaluation section in the paper detailing this use case evaluation.

\end{document}

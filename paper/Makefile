# based on https://tex.stackexchange.com/questions/40738/how-to-properly-make-a-latex-project

DOCNAME = paper

.PHONY: $(DOCNAME).pdf all clean

all: $(DOCNAME).pdf

$(DOCNAME).pdf: $(DOCNAME).lhs $(DOCNAME).bib sections/*.tex paper_bib.tex Formatting.fmt syntax-prelude.fmt
	lhs2TeX -o $(DOCNAME).tex --poly $(DOCNAME).lhs
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $(DOCNAME).tex -shell-escape

watch: $(DOCNAME).lhs $(DOCNAME).bib sections/*.tex paper_bib.tex Formatting.fmt syntax-prelude.fmt
	see $(DOCNAME).pdf &
	lhs2TeX -o $(DOCNAME).tex --poly $(DOCNAME).lhs
	latexmk -pvc -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $(DOCNAME).tex -shell-escape

clean:
	latexmk -CA || true
	rm -f $(DOCNAME).tex
	rm -f to_compile_processed.lhs
	rm -f pictures/*-eps-converted-to.pdf

compile: to_compile.lhs sections/*.tex
	flat-tex to_compile.lhs > to_compile_processed.lhs
	ghc -fno-code to_compile_processed.lhs

install:
	cp $(DOCNAME).pdf ${out}/$(DOCNAME).pdf

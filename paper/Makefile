# based on https://tex.stackexchange.com/questions/40738/how-to-properly-make-a-latex-project

DOCNAME = paper

.PHONY: $(DOCNAME).pdf all clean

all: $(DOCNAME).pdf

$(DOCNAME).pdf: $(DOCNAME).lhs $(DOCNAME).bib sections/*.tex Formatting.fmt syntax-prelude.fmt
	lhs2TeX -o $(DOCNAME).tex --poly $(DOCNAME).lhs
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $(DOCNAME).tex -shell-escape

watch: $(DOCNAME).lhs $(DOCNAME).bib sections/*.tex Formatting.fmt syntax-prelude.fmt
	see $(DOCNAME).pdf &
	lhs2TeX -o $(DOCNAME).tex --poly $(DOCNAME).lhs
	latexmk -pvc -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $(DOCNAME).tex -shell-escape

clean:
	latexmk -CA
	rm -f $(DOCNAME).tex

install:
	cp $(DOCNAME).pdf ${out}/$(DOCNAME).pdf

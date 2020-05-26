DOCNAME = paper

.PHONY: $(DOCNAME).pdf all clean

all: $(DOCNAME).pdf

$(DOCNAME).pdf: $(DOCNAME).tex $(DOCNAME).bib sections/*.tex paper_bib.tex Formatting.fmt syntax-prelude.fmt pictures/*.eps
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" $(DOCNAME).tex -shell-escape

clean:
	latexmk -CA || true


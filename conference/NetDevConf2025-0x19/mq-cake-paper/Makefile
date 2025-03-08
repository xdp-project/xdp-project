TEX = pdflatex
BIBTEX = bibtex
SVG2PDF = inkscape

MAIN = isea
TEXFILES = $(wildcard *.tex)
BIBFILES = $(wildcard *.bib)
SVGS = $(wildcard images/*.svg)
PDFS = $(SVGS:images/%.svg=svg-inkscape/%.pdf)

all: $(MAIN).pdf

$(MAIN).pdf: $(TEXFILES) $(BIBFILES) $(PDFS) $(PDF_TEXS)
	$(TEX) --shell-escape $(MAIN)
	$(BIBTEX) $(MAIN) || true
	$(TEX) $(MAIN)
	$(TEX) $(MAIN)

svg-inkscape/%.pdf: images/%.svg
	mkdir -p svg-inkscape
	$(SVG2PDF) --export-latex $< -o svg-inkscape/$*.pdf

clean:
	rm -f $(MAIN).aux $(MAIN).bbl $(MAIN).blg $(MAIN).log $(MAIN).out $(MAIN).pdf $(MAIN).toc $(MAIN).lof $(MAIN).lot $(MAIN).nav $(MAIN).snm $(MAIN).vrb
	rm -f svg-inkscape/*.pdf svg-inkscape/*.pdf_tex

.PHONY: all clean

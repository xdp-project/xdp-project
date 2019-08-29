# FIXME: This should be more generic and included everywhere; but works for now

BUILD_SCRIPT := ../emacs.d/build-slides.sh
BUILD_PDF_SCRIPT := ../build-pdf.sh
DECKTAPE := decktape --chrome-arg=--no-sandbox
HTML := xdp-distro-view.html
PDF := ${HTML:.html=.pdf}

html: $(HTML)
pdf: $(PDF)

all: html

$(HTML): %.html: %.org
	$(BUILD_SCRIPT) "$<"


$(PDF): %.pdf: %.html
	$(BUILD_PDF_SCRIPT) "$<" "$(DECKTAPE)"



CONF_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
BUILD_SCRIPT := $(CONF_DIR)emacs.d/build-slides.sh
BUILD_PDF_SCRIPT := $(CONF_DIR)build-pdf.sh

DECKTAPE_JS := $(wildcard ~/git/decktape/decktape.js)
DECKTAPE := $(if $(DECKTAPE_JS), node $(DECKTAPE_JS), decktape --chrome-arg=--no-sandbox)


HTML_FILES := ${ORG_FILES:.org=.html}
PDF_FILES := ${ORG_FILES:.org=.pdf}

html: $(HTML_FILES)
pdf: $(PDF_FILES)

all: html

$(HTML_FILES): %.html: %.org
	$(BUILD_SCRIPT) "$<"

$(PDF_FILES): %.pdf: %.html
	$(BUILD_PDF_SCRIPT) "$<" "$(DECKTAPE)"


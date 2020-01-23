
DESTDIR := build

all: export

.PHONY: export
export:
	./emacs.d/export-repo.sh $(DESTDIR)


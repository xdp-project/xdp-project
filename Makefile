
DESTDIR := build

all: export

.PHONY: export
export:
	./emacs.d/export-repo.sh $(DESTDIR)
	echo xdp-project.net > $(DESTDIR)/CNAME


SOURCE_DOCS := $(wildcard *.md)

EXPORTED_DOCS=\
 $(SOURCE_DOCS:.md=.html)

RM ?= /bin/rm -f

PANDOC ?= pandoc

PANDOC_HTML_OPTIONS=--standalone --highlight-style=tango --template tpl.html -f gfm --to html5

%.html : %.md
	$(PANDOC) $(PANDOC_HTML_OPTIONS) -o $@ $<


.PHONY: all clean

all : $(EXPORTED_DOCS)

clean:
	- $(RM) $(EXPORTED_DOCS)

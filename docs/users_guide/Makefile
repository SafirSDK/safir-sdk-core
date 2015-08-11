.SILENT:
.PHONY: all clean default

DEPS := users_guide.txt tracer.txt example_applications.txt \
		$(wildcard images/*.png) \
		$(patsubst %.dia,%.eps,$(wildcard images/*.dia)) \
		$(patsubst %.dia,%.png,$(wildcard images/*.dia)) \
		custom-coverpage-dblatex.sty \
		custom-xhtml11.css \
		Makefile

include ../../VERSION.txt
SAFIR_VERSION :=$(MAJOR).$(MINOR).$(PATCH)$(SUFFIX)

default: all

help:
	echo "Some available targets:"
	echo "   default                 - build pdf and html"
	echo "   users_guide.html - build html" 
	echo "   users_guide.pdf -  build pdf" 
	echo " Check the Makefile for other targets"

all: users_guide.pdf users_guide.html

vpath %.dia images
%.png: %.dia
	dia --export=$@ -t png-libart $<

%.eps: %.dia
	dia --export=$@ -t eps $<


users_guide.pdf: $(DEPS)
	echo "Building pdf"
	cp -f ../logo/renders/logotype1024.png .
	a2x -L -f pdf -d book --xsltproc-opts="--stringparam section.autolabel 1 --stringparam section.label.includes.component.label 1 --stringparam toc.section.depth 3" --dblatex-opts "-P doc.publisher.show=0 -P latex.output.revhistory=0 -s custom-coverpage-dblatex.sty" -a SAFIR_VERSION=$(SAFIR_VERSION) users_guide.txt 
	rm -f logotype1024.png

users_guide.html: $(DEPS)
	echo "Building html"
	cp -f ../logo/renders/logotype256.png images/
	asciidoc --backend=xhtml11  -a stylesheet="`pwd`/custom-xhtml11.css" -d book -a toc -a toclevels=4 -a numbered -a SAFIR_VERSION=$(SAFIR_VERSION) users_guide.txt

clean:
	rm -rf *~ *.pdf *.html *.fo *.png *.eps

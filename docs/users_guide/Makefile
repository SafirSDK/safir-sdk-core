.SILENT:
.PHONY: all clean default

DEPS := users_guide.txt olib.txt tracer.txt example_applications.txt \
		$(wildcard images/*.png) \
		$(patsubst %.dia,%.eps,$(wildcard images/*.dia)) \
		$(patsubst %.dia,%.png,$(wildcard images/*.dia)) \
	    users_guide-docinfo.xml \
		Makefile

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
	a2x -L -f pdf -d book -a docinfo --xsltproc-opts="--stringparam section.autolabel 1 --stringparam section.label.includes.component.label 1 --stringparam toc.section.depth 3" --dblatex-opts "-P latex.output.revhistory=0" users_guide.txt 

users_guide.html: $(DEPS)
	echo "Building html"
	asciidoc -d book -a toc -a toclevels=4 -a numbered users_guide.txt

clean:
	rm -rf *~ *.pdf *.html *.fo *.png *.eps

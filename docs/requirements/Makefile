.SILENT:
.PHONY: all clean

DEPS := Makefile requirements_specification.txt

default: all

help:
	echo "Check the Makefile for available targets"

all: requirements_specification.pdf requirements_specification.html

requirements_specification.pdf: $(DEPS)
	echo "Building Requirements Specification pdf"
	a2x -L -f pdf -d book --xsltproc-opts="--stringparam section.autolabel 1 --stringparam section.label.includes.component.label 1 --stringparam toc.section.depth 3" --dblatex-opts "-P latex.output.revhistory=0" requirements_specification.txt

requirements_specification.html: $(DEPS)
	echo "Building Requirements Specification html"
	asciidoc -d book -a toc -a toclevels=4 -a numbered requirements_specification.txt

clean:
	rm -rf *~ *.pdf *.html *.xml *.fo *.png

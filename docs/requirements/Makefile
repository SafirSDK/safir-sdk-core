.SILENT:
.PHONY: all clean

DEPS := Makefile sss.txt

default: all

help:
	echo "Check the Makefile for available targets"

all: sss.pdf sss.html

sss.pdf: $(DEPS)
	echo "Building SSS pdf"
	a2x -L -f pdf -d book --xsltproc-opts="--stringparam section.autolabel 1 --stringparam section.label.includes.component.label 1 --stringparam toc.section.depth 3" --dblatex-opts "-P latex.output.revhistory=0" sss.txt

sss.html: $(DEPS)
	echo "Building SSS html"
	asciidoc -d book -a toc -a toclevels=4 -a numbered sss.txt

clean:
	rm -rf *~ *.pdf *.html *.xml *.fo *.png

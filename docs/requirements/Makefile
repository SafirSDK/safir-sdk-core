.SILENT:
.PHONY: all clean

DEPS := Makefile requirements_specification.txt custom-coverpage-dblatex.sty custom-xhtml11.css

default: all

help:
	echo "Check the Makefile for available targets"

all: requirements_specification.pdf requirements_specification.html 

requirements_specification.pdf: $(DEPS)
	echo "Building Requirements Specification pdf"
	cp -f ../logo/renders/logotype1024.png .
	a2x -L -f pdf -d book --xsltproc-opts="--stringparam section.autolabel 1 --stringparam section.label.includes.component.label 1 --stringparam toc.section.depth 3" --dblatex-opts "-P latex.output.revhistory=0 -Pdoc.publisher.show=0 -s custom-coverpage-dblatex.sty" requirements_specification.txt
	rm -f logotype1024.png

requirements_specification.html: $(DEPS)
	echo "Building Requirements Specification html"
	cp -f ../logo/renders/logotype256.png .
#	a2x -L -f xhtml -d book --xsltproc-opts="--stringparam section.autolabel 1 --stringparam section.label.includes.component.label 1 --stringparam toc.section.depth 3" requirements_specification.txt
	asciidoc --backend=xhtml11  -a stylesheet=`pwd`/custom-xhtml11.css -d book -a toc -a toclevels=4 -a numbered requirements_specification.txt

clean:
	rm -rf *~ *.pdf *.html *.xml *.fo *.png

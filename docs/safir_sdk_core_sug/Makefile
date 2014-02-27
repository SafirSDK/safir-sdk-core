.SILENT:
.PHONY: all clean default

DEPS := safir_sdk_core_sug.txt olib.txt tracer.txt example_applications.txt $(wildcard images/*.png) Makefile block_diagram.eps block_diagram.png example_object.png example_object.eps safir_sdk_core_sug-docinfo.xml

default: all

help:
	echo "Some available targets:"
	echo "   default                 - build pdf and html"
	echo "   safir_sdk_core_sug.html - build html" 
	echo "   safir_sdk_core_sug.pdf -  build pdf" 
	echo " Check the Makefile for other targets"

all: safir_sdk_core_sug.pdf safir_sdk_core_sug.html

vpath %.dia images
%.png: %.dia
	dia --export=$@ -t png-libart $<

%.eps: %.dia
	dia --export=$@ -t eps $<


safir_sdk_core_sug.pdf: $(DEPS)
	echo "Building pdf"
	a2x -L -f pdf -d book -a docinfo --xsltproc-opts="--stringparam section.autolabel 1 --stringparam section.label.includes.component.label 1 --stringparam toc.section.depth 3" --dblatex-opts "-P latex.output.revhistory=0" safir_sdk_core_sug.txt 

safir_sdk_core_sug.html: $(DEPS)
	echo "Building html"
	asciidoc -d book -a toc -a toclevels=4 -a numbered safir_sdk_core_sug.txt

clean:
	rm -rf *~ *.pdf *.html *.fo *.png *.eps

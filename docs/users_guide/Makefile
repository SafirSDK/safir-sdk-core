.SILENT:
.PHONY: all clean

DEPS := safir_sdk_core_sug.txt olib.txt tracer.txt example_applications.txt $(wildcard images/*.jpg)

default: safir_sdk_core_sug.pdf

help:
	echo "Some available targets:"
	echo "   default                 - build pdf"
	echo "   all                     - build pdf, htmls and compressed archives"
	echo "   safir_sdk_core_sug.html - build single page html" 
	echo " Check the Makefile for other targets"

all: archives #will build other stuff due to deps

archives: safir_sdk_core_sug.pdf.zip \
	 safir_sdk_core_sug.pdf.bz2 \
	 safir_sdk_core_sug.single_html.zip \
	 safir_sdk_core_sug.single_html.tar.bz2 \
	 safir_sdk_core_sug.chunked_html.zip \
	 safir_sdk_core_sug.chunked_html.tar.bz2

safir_sdk_core_sug.pdf: $(DEPS)
	echo "Building pdf"
	a2x -L -f pdf -d book --xsltproc-opts="--stringparam section.autolabel 1 --stringparam section.label.includes.component.label 1 --stringparam toc.section.depth 3" safir_sdk_core_sug.txt 2>&1

#The grepping above is to get rid of some warnings from 
#some underlying component that cant handle some properties.
#(I believe this is fixed in some newer version of a2x 
#than the one I'm using.)

safir_sdk_core_sug.pdf.zip: safir_sdk_core_sug.pdf
	zip safir_sdk_core_sug.pdf.zip safir_sdk_core_sug.pdf

safir_sdk_core_sug.pdf.bz2: safir_sdk_core_sug.pdf
	bzip2 -9 -fk safir_sdk_core_sug.pdf

#docbook_html: 
#	a2x -f xhtml -d book safir_sdk_core_sug.txt

safir_sdk_core_sug.chunked/index.html: $(DEPS)
	echo "Building chunked html"
	a2x -L -f chunked -d book safir_sdk_core_sug.txt
	-mkdir -p safir_sdk_core_sug.chunked/images
	-cp -r images/*.jpg safir_sdk_core_sug.chunked/images/

safir_sdk_core_sug.chunked_html.zip: safir_sdk_core_sug.chunked/index.html
	zip -r safir_sdk_core_sug.chunked_html.zip safir_sdk_core_sug.chunked

safir_sdk_core_sug.chunked_html.tar.bz2: safir_sdk_core_sug.chunked/index.html
	tar cvfj safir_sdk_core_sug.chunked_html.tar.bz2 safir_sdk_core_sug.chunked/


safir_sdk_core_sug.html: $(DEPS)
	echo "Building html"
	asciidoc -d book -a toc -a toclevels=4 -a numbered safir_sdk_core_sug.txt

safir_sdk_core_sug.single_html.zip: safir_sdk_core_sug.html 
	zip safir_sdk_core_sug.single_html.zip safir_sdk_core_sug.html
	find images -name "*.jpg" -print | zip safir_sdk_core_sug.single_html.zip -@

safir_sdk_core_sug.single_html.tar.bz2: safir_sdk_core_sug.html 
	tar cfvj safir_sdk_core_sug.single_html.tar.bz2 safir_sdk_core_sug.html images/*.jpg

clean:
	rm -rf *~ *.pdf *.html *.xml *.fo safir_sdk_core_sug.chunked *.zip *.bz2

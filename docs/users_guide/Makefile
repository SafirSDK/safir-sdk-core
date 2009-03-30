.PHONY: pdf docbook_html all clean asciidoc_html chunked_html


all: pdf asciidoc_html chunked_html

pdf: 
	a2x -f pdf -d book --xsltproc-opts="--stringparam section.autolabel 1 --stringparam section.label.includes.component.label 1 --stringparam toc.section.depth 3" safir_sdk_core_sug.txt 2>&1 |grep -v "Ignoring property: border-right-color"|grep -v "Ignoring property: border-bottom-color" |grep -v "convertAttributeToProperty"

#The grepping above is to get rid of some warnings from 
#some underlying component that cant handle some properties.
#(I believe this is fixed in some newer version of a2x 
#than the one I'm using.)


docbook_html: 
	a2x -f xhtml -d book safir_sdk_core_sug.txt

chunked_html: 
	a2x -f chunked -d book safir_sdk_core_sug.txt
	-mkdir -p safir_sdk_core_sug.chunked/images
	-cp -r images/*.jpg safir_sdk_core_sug.chunked/images/

asciidoc_html:
	asciidoc -d book -a toc -a toclevels=4 -a numbered safir_sdk_core_sug.txt

clean:
	rm -rf *~ *.pdf *.html *.xml *.fo safir_sdk_core_sug.chunked
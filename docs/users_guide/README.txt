This document is written using the asciidoc markup language (see http://www.methods.co.nz/asciidoc/).

On a GNU/Linux system it is easy to install the toolchain needed to produce all kinds of documents. Simply install (package names may vary, this is for ubuntu) "asciidoc" and "source-highlight". You may also need to install "fop" and "docbook".

In Ubuntu I've had some problems with the syntax highlighting for the pdf targets. You may try to put the included source-highlight-filter.conf in /etc/asciidoc/filters/ and see if that makes things better.

For windows systems you may just want to download the asciidoc python script from the homepage, but if you haven't got source-highlight installed you'll have to do some editing to your asciidoc configuration script to get it working.

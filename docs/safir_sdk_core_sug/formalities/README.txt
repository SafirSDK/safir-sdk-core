This directory contains the files and tools to create a "Saab Approved" issue of this document. 
If you're not a Saab employee "blessed" with this task you can safely ignore this directory.

The makefile will take the PDF from the parent directory and replace the first two pages with the stuff in cover.pdf. 

If you're issuing a new version of this document, update the cover.doc, print it to PDF (using PDF Creator or something like it), and run "make" (this step probably has to be done on linux with ghostscript installed).




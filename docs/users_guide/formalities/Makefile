.PHONY: approved_pdf

#These are attempts at disabling any destructive image compression. Dont really know if they make any difference at all.
IMAGE_OPTIONS=-dAutoFilterColorImages=false -dAutoFilterGrayImages=false -dColorImageFilter=/FlateEncode -dGrayImageFilter=/FlateEncode -dDownsampleColorImages=false -dDownsampleGrayImages=false


approved_pdf:
	make -C .. safir_sdk_core_sug.pdf
	gs -q -sPAPERSIZE=a4 -dNOPAUSE -dBATCH $(IMAGE_OPTIONS) -sDEVICE=pdfwrite -dFirstPage=3 -sOutputFile=temp.pdf ../safir_sdk_core_sug.pdf
	gs -q -sPAPERSIZE=a4 -dNOPAUSE -dBATCH $(IMAGE_OPTIONS) -sDEVICE=pdfwrite -sOutputFile=safir_sdk_core_sug_approved.pdf cover.pdf temp.pdf
	rm -f temp.pdf

all: baseline.pdf chondro.pdf fileio.pdf flu.pdf introduction.pdf laser.pdf plotting.pdf clean 

chondro.pdf: 
	rm -f chondro.tex

fileio.pdf: 
	rm -f fileio.tex

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "library (tools); Sweave(\"$(<F)\"); texi2dvi(\"$(basename $(<F)).tex\", pdf = TRUE, clean = TRUE);"  
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen -dAutoRotatePages=/None \
	-dDownsampleColorImages=false -dNOPAUSE -dQUIET -dBATCH -dDownsampleColorImages=false \
	-sOutputFile=tmp.pdf $@ && qpdf tmp.pdf $@
	rm tmp.pdf
	rm -rf fig 
	rm -f $(basename $(<F)).tex
	rm -f Rplots.pdf

clean: 
	rm -f flu.rda laser.rda
	rm -rf fig 	
	rm -f Rplots.pdf
	rm -f *.tex

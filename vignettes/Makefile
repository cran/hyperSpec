ifndef R_HOME
	R_HOME=$(shell R RHOME)
endif
ifndef GSCMD
	GSCMD=$(shell which gs || which gswin32c)
endif

all: baseline.pdf chondro.pdf fileio.pdf flu.pdf introduction.pdf laser.pdf plotting.pdf
	@echo ${R_HOME} "\n"
	@echo ${R_GSCMD} "\n"

chondro.pdf: 
	rm -f chondro.tex

fileio.pdf: 
	rm -f fileio.tex

baseline.pdf: baseline.Rnw
	$(R_HOME)/bin/R CMD Sweave --pdf --encoding=utf8 $< 
	$(GSCMD) -sDEVICE=pdfwrite -dPDFSETTINGS=/screen -dAutoRotatePages=/None \
	-dDownsampleColorImages=false -dNOPAUSE -dQUIET -dBATCH  \
	-sOutputFile=tmp.pdf $@ && mv tmp.pdf $@ || echo "warning: gs not found"
	rm -rf fig 
	rm -f $(basename $(<F)).tex
	rm -f $(basename $(<F)).rda
	rm -f $(basename $(<F))-fig-*.*	
	rm -f Rplots.pdf

flu.pdf: flu.Rnw
	$(R_HOME)/bin/R CMD Sweave --pdf --encoding=utf8 $< 
	$(GSCMD) -sDEVICE=pdfwrite -dPDFSETTINGS=/screen -dAutoRotatePages=/None \
	-dDownsampleColorImages=false -dNOPAUSE -dQUIET -dBATCH  \
	-sOutputFile=tmp.pdf $@ && mv tmp.pdf $@ || echo "warning: gs not found"
	rm -rf fig 
	rm -f $(basename $(<F)).tex
	rm -f $(basename $(<F)).rda
	rm -f $(basename $(<F))-fig-*.*	
	rm -f Rplots.pdf

%.pdf: %.tex
#	$(R_HOME)/bin/R CMD Sweave --pdf --encoding=utf8 $< 
	$(R_HOME)/bin/Rscript -e "library (tools); texi2dvi('$<', clean = TRUE, index = TRUE, quiet = FALSE, pdf = TRUE);" 
#	$() -sDEVICE=pdfwrite -dPDFSETTINGS=/screen -dAutoRotatePages=/None \
	-dDownsampleColorImages=false -dNOPAUSE -dQUIET -dBATCH  \
	-sOutputFile=tmp.pdf $@ && qpdf tmp.pdf $@ && \
	rm tmp.pdf
	rm -rf fig 
	rm -f $(basename $(<F)).tex
	rm -f $(basename $(<F)).rda
	rm -f $(basename $(<F))-fig-*.*	
	rm -f Rplots.pdf

%.pdf: %.Rnw
	$(R_HOME)/bin/R CMD Sweave --pdf --encoding=utf8 $< 
#	$(R_HOME)/bin/Rscript -e "library (tools) ; Sweave ('$<', encoding = 'utf8') ;	texi2dvi(gsub ('.Rnw', '.tex', '$<'), clean = FALSE, index = TRUE, quiet = FALSE, pdf = TRUE)" 
	rm -f $(basename $(<F)).tex
	rm -f $(basename $(<F)).rda
	rm -f $(basename $(<F))-fig-*.*	
	rm -f Rplots.pdf

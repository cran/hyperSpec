all: baseline.pdf chondro.pdf fileio.pdf flu.pdf introduction.pdf laser.pdf plotting.pdf

chondro.pdf: 
	rm -f chondro.tex

fileio.pdf: 
	rm -f fileio.tex

baseline.pdf: baseline.Rnw
	"$(R_HOME)/bin/R" CMD Sweave --pdf --encoding=utf8 --compact-vignettes=both $< 
	rm -rf fig 
	rm -f $(basename $(<F)).tex
	rm -f $(basename $(<F)).aux
	rm -f $(basename $(<F)).bbl
	rm -f $(basename $(<F)).blg
	rm -f $(basename $(<F)).idx	
	rm -f $(basename $(<F)).ilg
	rm -f $(basename $(<F)).ind
	rm -f $(basename $(<F)).log
	rm -f $(basename $(<F)).out
	rm -f $(basename $(<F)).toc
	rm -f $(basename $(<F)).rda
	rm -f $(basename $(<F))*concordance.tex
	rm -f $(basename $(<F))-fig-*.*	

flu.pdf: flu.Rnw
	"$(R_HOME)/bin/R" CMD Sweave --pdf --encoding=utf8 --compact-vignettes=both $< 
	rm -rf fig 
	rm -f $(basename $(<F)).tex
	rm -f $(basename $(<F)).aux
	rm -f $(basename $(<F)).bbl
	rm -f $(basename $(<F)).blg
	rm -f $(basename $(<F)).idx
	rm -f $(basename $(<F)).ilg
	rm -f $(basename $(<F)).ind
	rm -f $(basename $(<F)).log
	rm -f $(basename $(<F)).out
	rm -f $(basename $(<F)).toc
	rm -f $(basename $(<F)).rda
	rm -f $(basename $(<F))*concordance.tex
	rm -f $(basename $(<F))-fig-*.*	

%.pdf: %.Rnw
	"$(R_HOME)/bin/R" CMD Sweave --pdf --encoding=utf8 --compact-vignettes=both $< 
	rm -f $(basename $(<F)).tex
	rm -f $(basename $(<F)).aux
	rm -f $(basename $(<F)).bbl
	rm -f $(basename $(<F)).blg
	rm -f $(basename $(<F)).idx
	rm -f $(basename $(<F)).ilg
	rm -f $(basename $(<F)).ind
	rm -f $(basename $(<F)).log
	rm -f $(basename $(<F)).out
	rm -f $(basename $(<F)).toc
	rm -f $(basename $(<F)).rda
	rm -f $(basename $(<F))-fig*.pdf
	rm -f $(basename $(<F))*concordance.tex
	rm -f $(basename $(<F))-fig-*.*	

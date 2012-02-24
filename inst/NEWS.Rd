\name{hyperSpec NEWS}
\title{hyperSpec News}
\encoding{UTF-8}

\section{Changes in Version 0.98}{
\subsection{User-visible changes}{
\itemize{
  \item new file import functions, (see vignette fileio)
  \itemize{
	 \item \code{scan.txt.Witec}: Witec ASCII files

	 \item \code{read.jdx.Shimadzu}: JCAMP-DX as produced by Shimadzu's GCxGC-MS
	 library (experimental) 

	 \item \code{read.txt.Shimadzu}: ASCII files exported by Shimadzu Chrome
	 Solution (v. 2.73)
  }

  \item hyperSpec gains a \code{subset} method

  \item levelplot (incl. plotmap) now uses `useRaster`. This allows
  better control for falling back to `panel.levelplot`.
  
  \item mixed colour map plots: \code{qplotmixmap} (experimental)
}
}
\subsection{Minor changes}{
  \itemize{
	 
	 \item \code{levelplot} uses \code{.wavelength} factors for subsetting, grouping,
	 conditioning: thus the strips will display the wavelength value.

	 \item extraction (\code{[]}) now warns if unused arguments are given

}
}

\subsection{Bug fixes}{
  \itemize{

	 \item bugfix: contour lines do not work with
	 \code{panel.levelplot.raster}.  hyperSpec now switches back to
	 \code{useRaster = FALSE}. Thanks to Giovanni Birarda for spotting this.
	 
	 \item bugfix for stacked plots: y axis labels were in wrong order in
	 some cases

	 \item recycling in initialize: columns may (again) be initialized with
	 a single value. Thanks to Raphael Viscarra-Rossel for spotting this.

	 \item bugfix for unsigned long int in .spc files 

	 \item stacked spectra plots label positions
}
}
\subsection{Internal changes}{
  \itemize{

\item Documentation format is now roxygen2
}
}
}
 


\name{hyperSpec NEWS}
\title{hyperSpec News}
\encoding{UTF-8}

\section{Changes in Version 0.98}{
\subsection{User-visible changes}{
\itemize{
  \item new file import functions, (see vignette fileio)
  \itemize{
	 \item \code{scan.txt.Witec}: Witec ASCII files (experimental: use
	 with care, also as different ways of Witec ASCII export produce
	 different file formats).

	 \item \code{read.jdx.Shimadzu}: JCAMP-DX as produced by Shimadzu's GCxGC-MS
	 library (experimental) 

	 \item \code{read.txt.Shimadzu}: ASCII files exported by Shimadzu Chrome
	 Solution (v. 2.73)
  }

  \item hyperSpec gains a \code{subset} method

  \item levelplot (incl. plotmap) now uses `useRaster`. This allows
  better control for falling back to `panel.levelplot`.
  
  \item mixed colour map plots: \code{qplotmixmap} (experimental)

  \item In addition to using \code{levelplot}, spectra matrices may be 
  plotted with the new base graphics function \code{plotmat}, which
  becomes the default for \code{plot (x, "mat")}.

  \item \code{apply (MARGIN = 2)} and \code{decomposition} work with
  more column formats now, e.g. \code{POSIXct} and \code{POSIXlt}.
  
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
	 \item 0.98-20120923: adapt \code{qplotspc} and \code{qplotmixmap} to
	 changes in ggplot2 0.9.2

	 \item \code{apply (MARGIN = 2)} reported the first row without
	 setting differing columns to \code{NA}. fixed 2012-05-13.

	 \item contour lines do not work with \code{panel.levelplot.raster}.
	 hyperSpec now switches back to \code{useRaster = FALSE}. Thanks to
	 Giovanni Birarda for spotting this.
	 
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
 


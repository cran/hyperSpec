\name{plotmap}
\alias{plotmap}
\alias{map.identify}

\title{Plot a Map and Identify Spectra}
\description{
  An image or map of a summary value of each spectrum is plotted. The
  spectra may be identified by mouse click.
}
\usage{
plotmap (object, x = "x", y = "y", func = mean, cond = NULL, 
         z = NULL, do.print = FALSE, print.args = NULL, trellis.args = NULL,
         \dots)
map.identify (object, x = "x", y = "y", \dots)
}

\arguments{
  \item{object}{the \code{hyperSpec} object}

  \item{x, y}{names of the columns in slot \code{@data} to be
	 used as x and y coordinates}
  
  \item{func}{function to apply to compute the summary statistic that is
	 displayed if \code{z} is not given.}
  
  \item{cond}{conditioning factor (for \code{\| cond} in the levelplot
  formula). May also be the name of the extra data column to be used for
  conditioning.}
  
  \item{z}{summary value to be displayed. If \code{z} is a
	 \code{factor}, and the default color scheme (returned by
	 \code{\link{matlab.palette}}) is used, one color per level of \code{z}
	 is used, otherwise a continuous palette is requested.
       }
  
  \item{do.print}{if \code{TRUE}, the lattice object is printed with the
	 further arguments given in \code{print.args} before returning it.}
  
  \item{print.args}{list with further arguments for printing the lattice
	 object.}
  
  \item{trellis.args}{list with further arguments passed to
	 \code{\link[lattice]{levelplot}}.}

  \item{\dots}{further arguments passed to \code{func} by
    \code{plotmap}.

    \code{map.identify} passes all further arguments to \code{plotmap}.
  }
}
\details{
  \code{plotmap} calculates a grid (via \code{\link{index.grid}}) on
  which the summary value is displayed with
  \code{\link[lattice]{levelplot}}. 

  Arguments to \code{\link[lattice]{levelplot}} can be given in
  \code{trellis.args}.

  \code{map.identify} calls \code{plotmap} and waits for (left) mouse
  clicks on points. The row indices of the
}
\value{
  a lattice object
}
\author{C. Beleites}
\seealso{
  \code{\link[lattice]{levelplot}} 
}
\examples{
background <- apply (chondro, 2, quantile, probs = 0.05)
corrected <- sweep (chondro, 2, background, "-")

plotmap (corrected, func = max)

plotmap (corrected, func = max,
         trellis.args = list (col.regions = gray ((100 : 0)/100))
        )


dist <- pearson.dist (chondro[[]])
dend <- hclust (dist, method = "ward")
z <- cutree (dend, h = 0.15)

plotmap (chondro, z = z)

plotmap (chondro, z = as.factor (z))
}
\keyword{hplot}


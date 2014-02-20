\name{qplotmap}
\alias{qplotmap}
\title{Spectra plotting with ggplot2}
\usage{
  qplotmap(object,
    mapping = aes_string(x = "x", y = "y", fill = "spc"),
    ..., func = mean, func.args = list(),
    map.tileonly = FALSE)
}
\arguments{
  \item{object}{hyperSpec object}

  \item{mapping}{see \code{\link[ggplot2]{geom_tile}}}

  \item{\dots}{handed to \code{\link[ggplot2]{geom_tile}}}

  \item{func}{function to summarize the wavelengths}

  \item{func.args}{arguments to \code{func}}

  \item{map.tileonly}{if \code{TRUE}, \code{mapping} will
  be handed to \code{\link[ggplot2]{geom_tile}} instead of
  \code{\link[ggplot2]{ggplot}}.}
}
\value{
  a \code{\link[ggplot2]{ggplot}} object
}
\description{
  Spectra plotting with ggplot2
}
\details{
  These functions are still experimental and may change
  substantially in future.

  Note that \code{qplotmap} will currently produce the
  wrong scales if x or y are discrete.
}
\examples{
qplotmap (chondro)
qplotmap (chondro) + scale_fill_gradientn (colours = alois.palette ())

## works also with discrete x or y axis:
qplotmap (chondro, mapping = aes (x = x, y = as.factor (y), fill = spc))
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link{plotmap}}

  \code{\link[ggplot2]{ggplot}}\code{\link[ggplot2]{geom_tile}}
}


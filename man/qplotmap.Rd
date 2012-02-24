\name{qplotmap}
\alias{qplotmap}
\title{Spectra plotting with ggplot2}
\usage{
  qplotmap(object,
    mapping = aes_string(x = "x", y = "y", fill = "spc"),
    ..., func = mean, func.args = list())
}
\arguments{
  \item{object}{hyperSpec object}

  \item{mapping}{see \code{\link[ggplot2]{geom_tile}}}

  \item{\dots}{handed to \code{\link[ggplot2]{geom_tile}}}

  \item{func}{function to summarize the wavelengths}

  \item{func.args}{arguments to \code{func}}
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
}
\examples{
qplotmap (chondro)
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link{plotmap}}

  \code{\link[ggplot2]{ggplot}}\code{\link[ggplot2]{geom_tile}}
}


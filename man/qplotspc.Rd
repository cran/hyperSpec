\name{qplotspc}
\alias{qplotspc}
\title{Spectra plotting with ggplot2}
\usage{
  qplotspc(x, wl.range, ...,
    mapping = aes_string(x = ".wavelength", y = "spc", group = ".rownames"),
    spc.nmax = 10, map.lineonly = FALSE)
}
\arguments{
  \item{x}{hyperSpec object}

  \item{wl.range}{wavelength ranges to plot}

  \item{\dots}{handed to \code{\link[ggplot2]{geom_line}}}

  \item{mapping}{see \code{\link[ggplot2]{geom_line}}}

  \item{spc.nmax}{maximum number of spectra to plot}

  \item{map.lineonly}{if \code{TRUE}, \code{mapping} will
  be handed to \code{\link[ggplot2]{geom_line}} instead of
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
}
\examples{
qplotspc (chondro)
  qplotspc (paracetamol, c (2800 ~ max, min ~ 1800)) + scale_x_reverse (breaks = seq (0, 3200, 400))

  qplotspc (aggregate (chondro, chondro$clusters, mean),
            mapping = aes (x = .wavelength, y = spc, colour = clusters)) +
    facet_grid (clusters ~ .)

  qplotspc (aggregate (chondro, chondro$clusters, mean_pm_sd),
            mapping = aes (x = .wavelength, y = spc, colour = clusters, group = .rownames)) +
    facet_grid (clusters ~ .)
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link{plotspc}}

  \code{\link[ggplot2]{ggplot}}\code{\link[ggplot2]{geom_line}}
}


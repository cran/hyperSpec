\name{index.grid}
\alias{index.grid}
\title{index.grid:  Calculate a Matrix with Indices for x,y-Data}
\description{
  An index matrix giving for each position of the grid index into
  \code{x} and \code{y} is calculated. In addidtion, two matrices
  similar to the matrices returned by \code{rows ()} and 
  \code{cols ()}, but giving the values for the x and y coordinates
  are calculated. 
}
\usage{
index.grid(x, y, z)
}
\arguments{
  \item{x}{Either a \code{hyperSpec} object or a vector giving the x
	 coordniates of the points.}
  \item{y}{A vector giving the y coordniates of the points (if \code{x}
	 is not a \code{hyperSpec} object).} 
  \item{z}{z values for the points}
}
\value{
  A list with
  
  \item{grid}{the index matrix: indices into x and y (or the rows of the
  \code{hyperSpec} object) for each point.}
  \item{x,y}{x and y coordinates, respectively, for each of the points.}
  \item{z}{if \code{z} was given as an argument, a matrix with the
  appropriate \code{z} value for each point of the grid.}
}
\author{C. Beleites}
\examples{
## select a small 4 x 4 corner of chondro
spc <- chondro[c(1, 2, 32, 33, 34, 35, 66, 67, 68,100, 101, 102)]
index.grid (spc, z = spc[[,,1000]])

index.grid (spc$x, spc$y)
}


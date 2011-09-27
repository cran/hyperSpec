\name{chondro}
\alias{chondro}
\alias{chondro}
\title{Raman spectra of 2 Chondrocytes in Cartilage...}
\description{Raman spectra of 2 Chondrocytes in Cartilage
A Raman-map (laterally resolved Raman spectra) of chondrocytes in
cartilage.}
\details{See the vignette.}
\docType{data}
\author{A. Bonifacio and C. Beleites}
\keyword{datasets}
\references{The raw data is available at \url{http://hyperspec.r-forge.r-project.org/chondro.zip}}
\examples{
chondro

## do baseline correction
baselines <- spc.fit.poly.below (chondro)
chondro <- chondro - baselines

## area normalization
chondro <- sweep (chondro, 1, apply (chondro, 1, mean), "/")

## substact common composition
chondro <- sweep (chondro, 2, apply (chondro, 2, quantile, 0.05), "-")

## PCA
pca <- prcomp (~ spc, data = chondro$., center = TRUE)
scores <- decomposition (chondro, pca$x, label.wavelength = "PC", label.spc = "score / a.u.")
loadings <- decomposition (chondro, t(pca$rotation), scores = FALSE, label.spc = "loading I / a.u.")

# remove outliers
out <- c(105, 140, 216, 289, 75, 69)
chondro <- chondro [- out]

# Hierarchical cluster analysis
dist <- dist (chondro [[]])
dendrogram <- hclust (dist, method = "ward")

plot (dendrogram)
clusters <- as.factor (cutree (dendrogram, k = 3))

cols <- c ("dark blue", "orange", "#C02020")
plotmap (chondro, clusters ~ x * y, col.regions = cols)

cluster.means <- aggregate (chondro, chondro$clusters, mean_pm_sd)
plot(cluster.means, stacked = ".aggregate", fill = ".aggregate", col = cols)

## plot nucleic acids
plotmap (chondro[, , c( 728, 782, 1098, 1240, 1482, 1577)],
col.regions = colorRampPalette (c("white", "gold", "dark green"), space = "Lab") (20))

\dontrun{vignette ("chondro", package = "hyperSpec")}
}


\name{olive}
\alias{olive}
\docType{data}
\title{Olive Oil Data Set}
\description{
  Data set consisting of 572 rows and 10 columns containing data about
  olive oil.
}
\usage{
data("olive")
}
\format{
  A \code{\link{data.frame}()} with 10 columns:
  \describe{
    \item{\code{Area}:}{(larger) area.}
    \item{\code{Region}:}{(local) region.}
    \item{\code{palmitic}, \code{palmitoleic}, \code{stearic},
      \code{oleic}, \code{linoleic}, \code{linolenic}, \code{arachidic},
      \code{eicosenoic}:}{the fatty acids measured.}
  }
}
\source{
  The data set was obtained from the package \pkg{pdfCluster} (for
  convenience). It contains 572 rows of observations.  The first and the
  second column correspond to the area (Centre-North, South,
  Sardinia) and the geographical region of origin of the olive oils
  (northern Apulia, southern Apulia, Calabria, Sicily, inland Sardinia
  and coast Sardinia, eastern and western Liguria, Umbria),
  respectively. The remaining columns represent the chemical
  measurements (on the acid components for the oil specimens) palmitic,
  palmitoleic, stearic, oleic, linoleic, linolenic, arachidic,
  eicosenoic.
}
\examples{
data("olive")
}
\keyword{datasets}
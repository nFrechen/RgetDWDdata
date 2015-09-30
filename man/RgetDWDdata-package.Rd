\name{RgetDWDdata-package}
\alias{RgetDWDdata-package}
\alias{RgetDWDdata}
\docType{package}
\title{
R functions to download data from the DWD (German Weather Service)
}
\description{
Dieses Paket enthält Funktionen zum herunterladen von DWD (Deutscher Wetterdienst) Klimadaten und zum Berechnen von hydrologischen Kennwerten und Wasserhaushaltsgrößen.
}
\details{
\tabular{ll}{
Package: \tab RgetDWDdata\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2015-01-09\cr
License: \tab GPL-2\cr
}
}
\author{
Nanu Frechen, Egbert Gassert, Ina Pohle, Tobias Recke

Maintainer: Nanu Frechen <nanu.frechen@b-tu.de>
}
\references{
Quelle der DWD Daten: \url{http://www.dwd.de}

Die Funktion \code{\link{getDWDdata}} wurde inspiriert von der Funktion \code{\link[EcoHydRology]{get_usgs_gage}} des \code{\link[EcoHydRology]{EcoHydRology}} Pakets.
}
\keyword{ package }
\seealso{
\code{\link[EcoHydRology]{EcoHydRology}}
}
\examples{
KlimadatenCottbus <- getDWDdata("Cottbus") 
# lädt den aktuellen Datensatz der Station Cottbus herunter

}

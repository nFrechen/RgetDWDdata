\name{net.radiation}
\alias{net.radiation}
\title{Berechnet die Nettostrahlung}
\description{
Berechnet das Verdunstungsäquivalent der Nettostrahlung in mm/d nach dem ATV-DVWK-Merkblatt 504. Dieses wird für die Berechnung der Grasreferenzverdunstung (siehe \code{\link{calcET0}}) benötigt.
}
\usage{
net.radiation(sunshine, temp, U, date, latitude, albedo)
}
\arguments{
  \item{date}{Datum (Tag der Messung)}
  \item{latitude}{Breitengrad der Messstation}
  \item{sunshine}{Sonnenscheindauer in Stunden}
  \item{temp}{Lufttemperatur}
  \item{U}{relative Luftfeuchte}
  \item{albedo}{Albedo der beschienen Oberfläche (z.B. Gras)}
}
\value{
Gibt das Verdunstungsäquivalent der Nettostrahlung in mm/d zurück
}
\references{
ATV-DVWK-Merkblatt 504: Verdunstung in Bezug zu Landnutzung, Bewuchs und Boden, Hennef 2002
}
\author{
Nanu Frechen
}

\seealso{
\code{\link{calcET0}}}
\examples{

}

\name{evap.net.radiation}
\alias{evap.net.radiation}
\title{Berechnet das Verdunstungsäquivalent der Nettostrahlung}
\description{
Berechnet das Verdunstungsäquivalent der Nettostrahlung in mm/d nach dem \href{http://www.dwa.de/dwa/shop/shop.nsf/Produktanzeige?openform&produktid=P-DWAA-785JW8}{ATV-DVWK-Merkblatt 504}. Dieses wird für die Berechnung der Grasreferenzverdunstung (siehe \code{\link{calcET0}}) benötigt.
}
\usage{
evap.net.radiation(R_G=NA, sunshine=NA, doy=NA, date=NA, temp, U, latitude, albedo=0.23)
}
\arguments{
  \item{R_G}{Globalstrahlung in J/(cm^2*d). Muss gegeben sein wenn \code{sunshine} nicht gegeben ist. Wird benutzt wenn sowohl \code{R_G} als auch \code{sunshine} gegeben sind.}
  \item{sunshine}{Sonnenscheindauer in Stunden. Muss gegeben sein wenn \code{R_G} nicht gegeben ist (Globalstrahlung wird aus Sonnenscheindauer berechnet). Wird nicht benutzt wenn \code{R_G} gegeben ist.}
	\item{date}{Datum der Messung. Muss gegeben sein wenn \code{doy} nicht gegeben ist.}
  \item{doy}{Tag des Jahres (day of year). Muss gegeben sein wenn \code{date} nicht gegeben ist.}
  \item{temp}{Lufttemperatur in °C}
  \item{U}{relative Luftfeuchte in Prozent}
  \item{latitude}{Breitengrad der Messstation}
  \item{albedo}{Albedo der beschienen Oberfläche (z.B. Gras)}
}
\value{
Gibt das Verdunstungsäquivalent der Nettostrahlung in mm/d zurück
}
\references{
\href{http://www.dwa.de/dwa/shop/shop.nsf/Produktanzeige?openform&produktid=P-DWAA-785JW8}{ATV-DVWK-Merkblatt 504: Verdunstung in Bezug zu Landnutzung, Bewuchs und Boden, Hennef 2002}
}
\author{
Nanu Frechen
}

\seealso{
Berechnung der Grasreferenzverdunstung \code{\link{calcET0}}} und des Sättigungsdampfdrucks \code{\link{es}}
\examples{

}

\name{es}
\alias{es}
\alias{s}
\title{
Sättigungsdampfdruck
}
\description{
Berechnet den Sättigungsdampfdruck in hPa über Wasser oder Eis und die Steigung der Sättigungsdampdruckkurve in hPa/K nach \href{http://www.dwa.de/dwa/shop/shop.nsf/Produktanzeige?openform&produktid=P-DWAA-785JW8}{ATV-DVWK-Merkblatt 504}. Wird für die Berechnung der Grasreferenzverdunstung (siehe \code{\link{calcET0}}) verwendet.
}
\usage{
es(temp, ice=FALSE)
s(temp, ice=FALSE)
}
\arguments{
  \item{temp}{Lufttemperatur als Tagesmittel in °C}
	\item{ice}{Berechnung über Eis \code{=TRUE} oder über Wasser \code{=FALSE} (Standard)}
}
\details{
Für Wasser muss die Lufttemperatur zwischen -45°C und +65°C liegen. Für Eis zwischen -65°C und +0.01°C
}

\value{
\code{es()} gibt den Sättigungsdampfdruck in hPa zurück.

\code{s()} gibt die Steigung der Sättigungsdampfdruckkurve in hPa/K zurück.
}

\references{
\href{http://www.dwa.de/dwa/shop/shop.nsf/Produktanzeige?openform&produktid=P-DWAA-785JW8}{ATV-DVWK-Merkblatt 504: Verdunstung in Bezug zu Landnutzung, Bewuchs und Boden, Hennef 2002}
}

\seealso{\code{\link{calcET0}}}

\examples{
Temp_Wasser <- -45:65 #°C
Temp_Eis <- -65:0 # °C

plot(Temp_Wasser, es(Temp_Wasser), type="l", col="blue", xlim=c(-65,65),
	lwd=3, xlab="Temperatur [°C]", ylab="Sättigungsdampfdruck [hPa]", 
	main="Sättigungsdampfdruck Kurve")
lines(Temp_Eis, es(Temp_Eis), col="lightblue", lwd=3, lty=2)
legend("topleft", c("Wasser", "Eis"), title = "über", 
	col = c("blue", "lightblue"), lty = 1:2, lwd=3)
}

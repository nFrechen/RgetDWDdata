\name{calcET0}
\alias{calcET0}
\title{Grasreferenzverdunstung}
\description{
Berechnet die Grasreferenzverdunstung (ET0) nach \href{http://www.dwa.de/dwa/shop/shop.nsf/Produktanzeige?openform&produktid=P-DWAA-785JW8}{ATV-DVWK-Merblatt 504}}
\usage{
calcET0(temp, date=NA, doy=NA, R_G=NA, sunshine=NA, U, v, v_height=2, latitude, albedo=0.23, gamma=0.65, ice = FALSE)
}
\arguments{
  \item{temp}{Lufttemperatur als Tagesmittel in °C}
  \item{date}{Datum der Messung. Muss gegeben sein wenn \code{doy} nicht gegeben ist.}
  \item{doy}{Tag des Jahres (day of year). Muss gegeben sein wenn \code{date} nicht gegeben ist.}
  \item{R_G}{Globalstrahlung in J/(cm^2*d). Muss gegeben sein wenn \code{sunshine} nicht gegeben ist. Wird benutzt wenn sowohl \code{R_G} als auch \code{sunshine} gegeben sind.}
  \item{sunshine}{Sonnenscheindauer in Stunden. Muss gegeben sein wenn \code{R_G} nicht gegeben ist (Globalstrahlung wird aus Sonnenscheindauer berechnet). Wird nicht benutzt wenn \code{R_G} gegeben ist.}
  \item{U}{Relative Luftfeuchte in Prozent}
  \item{v}{Windgeschwindigkeit als Tagesmittel in m/s}
  \item{v_height}{Höhe der Windgeschwindigkeitsmessung in m (wird in die Wingeschwindigkeit in 2m Höhe umgerechnet)}
	\item{latitude}{Breitengrad der Messstation}
	\item{albedo}{Albodo der Vegetation (Standard = 0.23 für Gras)}
  \item{gamma}{Psychrometerkonstante (Standard = 0.65 hPa/K)}
  \item{ice}{Soll die Verdunstung über Eis oder flüssigem Wasser berechnet werden?}
}
\details{
Die Gras-Referenzverdunstung basiert auf der Penman-Monteith-Beziehung. Sie ist der international einheitliche Standard zur Berechnung der potenziellen Verdunstung.

Randbedingungen sind:
\itemize{
\item{Der Boden ist das ganze Jahr über einheitlich mit Gras von 12 cm Höhe bedeckt.}
\item{Es ist in die Penman-Monteith-Beziehung ein von der Windgeschwindigkeit in 2 m Höhe abhängiger aerodynamischer Verdunstungswiderstand von ra = 208/v2 einzusetzen, angegeben in s/m.}
\item{Dabei ist v2 die in 2 m Höhe gemessene Windgeschwindigkeit. Die Wind-Beziehung gilt streng genommen nur bei neutraler Schichtung in der unteren Atmosphäre, kann bei den geforderten Genauigkeiten hier aber generell angesetzt werden.}
\item{Es besteht kein Trockenstress für die Pflanzen. Diese Bedingung ist im Winter und Frühjahr sowie in feuchten Sommern erfüllt, wenn im Hauptwurzelraum die relative Bodenfeuchte Wrel größer als etwa 50 bis 60 \% der nutzbaren Feldkapazität ist. Für Verdunstungsbedingungen ohne Trockenstress wird als minimaler Bestandswiderstand rc,min = 70 s/m angesetzt.}
\item{Zur Berechnung der Strahlungsbilanz aus der Globalstrahlung wird einheitlich eine Albedo von  = 0,23 festgelegt, die im Mittel für Grasbewuchs und andere grüne Pflanzenbestände gilt, aber nicht generell für alle Oberflächen. }
}

Die überall unterschiedliche Bodenbedeckung (die Art der Landnutzung, Schneedecke, Wasserflächen, Siedlungen usw.) sowie die Bodenart sind bei der Gras-Referenzverdunstung nicht berücksichtigt. Auch gehen die unterschiedliche Wasserversorgung der Pflanzen, die Wasserspeicherfähigkeit der Böden und der kapillare Aufstieg aus dem Grundwasser nicht ein. 
}

\value{
Gibt die Grasreferenzverdunstung (ET0) in mm/d zurück.
}
\references{
\href{http://www.dwa.de/dwa/shop/shop.nsf/Produktanzeige?openform&produktid=P-DWAA-785JW8}{ATV-DVWK-Merkblatt 504: Verdunstung in Bezug zu Landnutzung, Bewuchs und Boden, Hennef 2002}
}
\author{
Nanu Frechen, Sabine Schümberg
}


\seealso{
Verwendet die Berechnung des Verdunstungsequivalents der Nettostrahlung \code{\link{evap.net.radiation}} und des Sättigungsdampfdrucks \code{\link{es}}
}
\examples{
Temp <- 30 # °C
U <- 60 # %
v_2 <- 3 # m/s
Sonnenscheindauer <- 10 # h
Breitengrad <- 51 # °
Datum <- as.Date("2014-06-01")

calcET0(temp = Temp, date = Datum, sunshine = Sonnenscheindauer, 
	U = U, v = v_2, latitude = Breitengrad) 
# 6.187067

#-----------------------------------

# Klimadaten herunterladen:
Cottbus <- getDWDdata("Cottbus", historisch = NA, Metadaten = TRUE)

Temp <- Cottbus$Daten$LUFTTEMPERATUR
Datum <- Cottbus$Daten$MESS_DATUM
Sonnenschein <- Cottbus$Daten$SONNENSCHEINDAUER
U <- Cottbus$Daten$REL_FEUCHTE       
Wind <- Cottbus$Daten$WINDGESCHWINDIGKEIT
Wind_h <- 16 #m
Breitengrad <- tail(Cottbus$Metadaten$Geogr.Breite, 1)
Niederschlag <- Cottbus$Daten$NIEDERSCHLAGSHOEHE

# Grasreferenzverdunstung berechnen:
ET0 <- calcET0(temp = Temp, date = Datum, sunshine = Sonnenschein, U = U, v = Wind, v_height = Wind_h, latitude = Breitengrad)
# Zeitreihe für das letze Jahr
index <- (length(Datum)-365):length(Datum)
plot(Datum[index], ET0[index], type="l", main="Grasreferenzverdunstung im Jahresverlauf")
plot(Datum[index], Niederschlag[index]-ET0[index], type="l", main="Klimatische Wasserbilanz")

# Streudiagriamm für die gesamte Messperiode
plot(yday(Datum), ET0, pch=20, cex=0.2, main="Grasreferenzverdunstung im Jahresverlauf")
plot(yday(Datum), Niederschlag-ET0, pch=20, cex=0.2, main="Klimatische Wasserbilanz")
}

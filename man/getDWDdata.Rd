\name{getDWDdata}
\alias{getDWDdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lädt Klimadaten von den Servern des DWD herunter
}
\description{
Der Deutsche Wetterdienst (DWD) stellt unter \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl}{www.dwd.de} eine Reihe von Klimadaten (Tagesdaten) zur Verfügung. Diese können mit dieser Funktion heruntergeladen und in R importiert werden.
}
\usage{
getDWDdata(Messstelle, historisch=FALSE, Metadaten=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Messstelle}{
	Name der Messstelle oder Stations-Kennziffer, wie sie in \code{\link{getDWDstations}} zu finden sind.
}
\item{historisch}{
	Wenn TRUE wird der historische Datensatz heruntergeladen (geht für gewöhnlich bis zum 31. Dezember des Vorjahres), wenn FALSE wird der aktuelle Datensatz (für gewöhnlich die letzten 365 Tageswerte) heruntergeladen. NA lädt beide Datensätze herunter und fügt sie zusammen. Damit ergibt sich ein lückenloser Datensatz vom Beginn der Messung bis ein bis zwei Tage vor dem aktuellen Datum.
}
\item{Metadaten}{
	(logical) Soll der zurückgegebene Datensatz die vom DWD mitgelieferten Metadaten enthalten?
}
}
\details{
Eine Beschreibung der Einheiten und Messprozeduren findet sich \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/BESCHREIBUNG_obsgermany_climate_daily_kl_recent_de.pdf}{hier (Deutsch)} oder \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/DESCRIPTION_obsgermany_climate_daily_kl_recent_en.pdf}{hier (Englisch)}. Weitere Informationen finden sich in den Zusatzinfos, die mit den Datensätzen heruntergeladen werden können (\code{Metadaten=TRUE}).
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
Quelle der Daten: \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl}{www.dwd.de}


Diese Funktion wurde inspiriert von der Funktion \code{\link{get_usgs_gage}} des \code{\link{EcoHydRology}} Pakets.
}
\author{
Nanu Frechen, Tobias Recke
}
\note{
Bei einigen Messstationen kann die Datenstruktur inkonsistent sein und deshalb nicht von dieser Funktion eingelesen werden. Darüber hinaus ändert der DWD manchmal seine Datenstrukturen, was ein Anpassen dieser Funktion erfordert. Wenn Sie etwas entdecken informieren sie den Maintainer dieses Pakets (\href{mailto: nanu.frechen@b-tu.de}{nanu.frechen@b-tu.de}).
Die Stationen Aachen, Berlin-Dahlem (LFAG), Karlsruhe, Leuchtturm Alte Weser und Rostock-Warnemünde sind bekannt Fehler beim herunterladen zu erzeugen (siehe Beispiel in \code{\link{getDWDstations}}).
}


\seealso{
\code{\link{getDWDstations}}, \code{\link{updateDWDdata}}
}
\examples{
KlimadatenCottbus <- getDWDdata("Cottbus") 
# lädt den aktuellen Datensatz der Station Cottbus herunter
head(KlimadatenCottbus)
tail(KlimadatenCottbus)

KlimadatenHannover <- getDWDdata("Hannover", historisch=TRUE, Metadaten=TRUE) 
# lädt den historischen Datensatz der Station Hamburg herunter
head(KlimadatenHannover)
tail(KlimadatenHannover)


KlimadatenCottbus <- getDWDdata("10496", historisch=NA, Metadaten=TRUE) 
# lädt den historischen und aktuellen Datensatz der Station Cottbus herunter 
# (über die Stations-Kennziffer) und gibt zusätzlich Metadaten zurück
str(KlimadatenCottbus, 1)
head(KlimadatenCottbus$Daten)
tail(KlimadatenCottbus$Daten)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

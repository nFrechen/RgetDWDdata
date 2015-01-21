\name{getDWDstations}
\alias{getDWDstations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lädt die Liste verfügbarer Wetterstationen des DWD herunter 
}
\description{
Die Liste enthält Stationsnamen, Stations-Kennziffer, Stationshöhe, geografische Breite und Länge und den Beginn der Klimareihe.
}
\usage{
getDWDstations()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
Braucht keine Argumente
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\references{
Quelle der Daten: \url{http://www.dwd.de}
}
\author{
Nanu Frechen
}
%\note{
%%  ~~further notes~~
%}


\seealso{
\code{\link{getDWDdata}}, \code{\link{updateDWDdata}}
}
\examples{
# Stationsliste herunterladen:
Stationen <- getDWDstations()
View(Stationen)


# Stationen nach Länge der Messreihe sortieren:
Stationen$"Länge Messreihe (Jahre)" <- 
	year(Sys.time()) - as.numeric(Stationen$"Beginn Klimareihe")
View(Stationen[order(Stationen$"Länge Messreihe (Jahre)"),])


# aktuelle Daten aller Stationen herunterladen:
DWDdata <- list()
for(i in (1:length(Stationen$Stationsname))){
	DWDdata[[Stationen$Stationsname[i]]] <- 
		try(getDWDdata(Stationen$"Stations-Kennziffer"[i],
			Metadaten = T))
}


# Struktur der erzeugten Liste anzeigen:
str(DWDdata, 1, vec.len=0, give.attr = FALSE)


# Fehlerhafte Stationen aus der Liste nehmen:
Fehlerhaft <- which(lapply(DWDdata, class)=="try-error")
print(Fehlerhaft)
Stationen <- Stationen[-Fehlerhaft,]
}
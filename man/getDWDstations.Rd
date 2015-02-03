\name{getDWDstations}
\alias{getDWDstations}
\title{Lädt die Liste verfügbarer Wetterstationen des DWD herunter}
\description{
Die Liste enthält Stationsnamen, Stations-Kennziffer, Stationshöhe, geografische Breite und Länge und den Beginn der Klimareihe.
}
\usage{
getDWDstations()
}
\arguments{
Braucht keine Argumente
}

\references{
Quelle der Daten: \url{http://www.dwd.de}

Die Stationsliste wird von \href{http://www.dwd.de/sid_gCpjSTGJDhT7rZvV38t3vSJWnnQc1HLyFcD46pL789crw0MpqGrg!295356740!-364271037!1385038953230/bvbw/appmanager/bvbw/dwdwwwDesktop?_nfpb=true&_pageLabel=dwdwww_result_page&portletMasterPortlet_i1gsbDocumentPath=Navigation/Oeffentlichkeit/Klima__Umwelt/Klimadaten/kldaten__kostenfrei/stations_C3_BCbersicht__tabelle__node.html?F__nnn=true}{dieser url} heruntergeladen.
}
\author{
Nanu Frechen
}


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
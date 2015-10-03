\name{getDWDstations}
\alias{getDWDstations}
\title{Lädt die Liste verfügbarer Wetterstationen des DWD herunter}
\description{
Die Liste enthält Stationsnamen, Stations-Kennziffer, Stationshöhe, geografische Breite und Länge und den Beginn der Klimareihe.
}
\usage{
getDWDstations(type="daily")
}
\arguments{
  \item{type}{
	either "daily" or "hourly" to ask for stations with available daily or hourly data
	}
}

\references{
Quelle der Daten: \url{http://www.dwd.de}

Die Stationsliste der täglichen Daten wird von \href{http://www.dwd.de/sid_gCpjSTGJDhT7rZvV38t3vSJWnnQc1HLyFcD46pL789crw0MpqGrg!295356740!-364271037!1385038953230/bvbw/appmanager/bvbw/dwdwwwDesktop?_nfpb=true&_pageLabel=dwdwww_result_page&portletMasterPortlet_i1gsbDocumentPath=Navigation/Oeffentlichkeit/Klima__Umwelt/Klimadaten/kldaten__kostenfrei/stations_C3_BCbersicht__tabelle__node.html?F__nnn=true}{dieser url} heruntergeladen.


Die Stationsliste der stündlichen Daten wird von \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/recent/RR_Stundenwerte_Beschreibung_Stationen.txt}{dieser url} heruntergeladen.
}
\author{
Nanu Frechen, Tobias Recke
}


\seealso{
\code{\link{getDWDdata}}, \code{\link{getDWDdataHourly}}, \code{\link{updateDWDdata}}
}
\examples{
# Stationsliste herunterladen:
Stationen <- getDWDstations(type="daily")
print(Stationen)


# Stationen nach Länge der Messreihe sortieren:
Stationen$"Länge Messreihe (Jahre)" <- 
	year(Sys.time()) - as.numeric(Stationen$"Beginn Klimareihe")
print(Stationen[order(Stationen$"Länge Messreihe (Jahre)"),])


if(interactive()){
	# aktuelle Daten aller Stationen herunterladen:
	readline("Wollen Sie wirklich Daten aller Stationen herunterladen (dies kann eine Weile dauern [Enter/Esc]")
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

}

# Karte mit allen Stationen anzeigen:
if(require("maps")){
	map("world", regions = "Germany")
	points(stations[,6:5], pch=20, cex=0.5)
	if(interactive())
	stations[ identify(stations[,6:5], label=stations$Stationsname, n=1, col="red"), ]
}

}
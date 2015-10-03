getDWDstations <- function(type="daily"){
	switch(type,
				 daily = {
						#library("XML") # not needed in package
						DWDstationenURL <- "http://www.dwd.de/sid_gCpjSTGJDhT7rZvV38t3vSJWnnQc1HLyFcD46pL789crw0MpqGrg!295356740!-364271037!1385038953230/bvbw/appmanager/bvbw/dwdwwwDesktop?_nfpb=true&_pageLabel=dwdwww_result_page&portletMasterPortlet_i1gsbDocumentPath=Navigation%2FOeffentlichkeit%2FKlima__Umwelt%2FKlimadaten%2Fkldaten__kostenfrei%2Fstations_C3_BCbersicht__tabelle__node.html%3F__nnn%3Dtrue"
						Stationen <- readHTMLTable(DWDstationenURL, stringsAsFactors = FALSE, which=7, skip.rows=1, header=TRUE, colClasses = c("character", "character", "character", "character", "numeric", "character", "character", "character", "numeric"))
						colnames(Stationen) <- sub("\n\n", " ", colnames(Stationen))
						colnames(Stationen)[6:7] <- c("geoBreite", "geoLaenge")
						Stationen$"geoBreite" <- sub("°.", ".", Stationen$"geoBreite")
						Stationen$"geoBreite" <- as.numeric(sub("'", "", Stationen$"geoBreite"))
						Stationen$"geoLaenge" <- sub("°.", ".", Stationen$"geoLaenge")
						Stationen$"geoLaenge" <- as.numeric(sub("'", "", Stationen$"geoLaenge"))
						
						return(Stationen)
				 }, 
			   hourly={
						colnames_stationen <- as.vector(t(read.table(url("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/recent/RR_Stundenwerte_Beschreibung_Stationen.txt", encoding="ISO-8859-1"), nrows = 1)))
						
						stationen <- read.fwf(url("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/recent/RR_Stundenwerte_Beschreibung_Stationen.txt", encoding="ISO-8859-1"), widths = c(5,9,9,15,12,10,42,23), skip=2, col.names = colnames_stationen, strip.white=T, colClasses=c("character", "character", "character", "integer", "numeric", "numeric", "character", "character"))
						stationen$von_datum <- as.Date(as.character(stationen$von_datum), format = "%Y%m%d")
						stationen$bis_datum <- as.Date(as.character(stationen$bis_datum), format = "%Y%m%d")
						str(stationen)
						return(head(stationen, -1))
				 },
					stop('type must be either "daily" or "hourly')
	)
}



getDWDdata <- function(Messstelle, historisch=FALSE, Metadaten=FALSE){
	
	# "Messstelle" muss entweder die 5-stellige Stations-Kennziffer sein oder der Stationsame als character
	# welche muss entwerder "historisch" oder "aktuell" sein
	# historisch=TRUE laed die historischen Daten herunter (reichen fuer gewoehnlich vom Beginn der Messungen bis zum letzten Jahreswechsel)
	# historisch=FALSE laed die aktuellen Daten vom letzten Jahreswechsel bis gestern herunter
	# historisch=NA kombiniert die beiden Datensaetze
	
	#library("XML") # not needed in package
	
	if(!is.na(historisch)){
		#if(historisch){
		#	Metadaten=FALSE
		#}	
	}
	
	
	if(is.na(suppressWarnings(as.numeric(Messstelle)))) {
		# downloade Stationsliste wenn "Messstelle" ein character ist (Stationsname)
		stationen <- getDWDstations()
		Messstelle_nr <- stationen$`Stations-Kennziffer`[which(stationen$Stationsname==Messstelle | stationen$`Stations-Kennziffer`==Messstelle)]
		if(length(Messstelle_nr)==0) stop(paste0('Messstelle "', Messstelle, '" kann nicht gefunden werden'))
		Messstelle <- Messstelle_nr
	}
	# Ansonsten einfach die Nummer verwenden
	
	historisch <- as.logical(historisch)
	
	# kombiniert die beiden Datensaetze, wenn historisch=NA:
	if(is.na(historisch)){
		aktuell <- getDWDdata(Messstelle, historisch=FALSE, Metadaten = TRUE)
		historisch <- getDWDdata(Messstelle, historisch=TRUE, Metadaten = FALSE)
		
		# historisch und aktuell zusammenfuegen
		
		# letztes Datum 1.Datensatz
		letztesDatum <- historisch$MESS_DATUM[ nrow(historisch) ]
		
		# erstes Datum 2.Datensatz
		erstesDatum <- aktuell$Daten$MESS_DATUM[1]
		
		# ergibt Zeilennummer von letztesDatum im aktuellen Datensatz
		zl.nr <- which(letztesDatum==aktuell$Daten$MESS_DATUM)
		
		# Am Jahresanfang fehlen manchmal ein paar Daten:
		if(length(zl.nr)==0){
			zl.nr=0
			Fehltage <- seq(letztesDatum+days(1), erstesDatum-days(1), 1)
			indexFehltage <- nrow(historisch)+1:length(Fehltage)
			historisch[indexFehltage,1:2] <- data.frame(historisch$STATIONS_ID[1], Fehltage)
		} 
		
		# verbindet 1. und 2. Datensatz
		aktuell$Daten <- 
			test <- rbind(historisch,aktuell$Daten[(zl.nr+1):nrow(aktuell$Daten) , ])
		if(Metadaten){
			return(aktuell)
		}else{
			return(aktuell$Daten)
		}
	}
	
	if(historisch){
		downloadlink <- paste0("http://www.dwd.de/bvbw/generator/DWDWWW/Content/Oeffentlichkeit/KU/KU2/KU21/klimadaten/german/download/tageswerte/kl__", Messstelle, "__hist__txt,templateId=raw,property=publicationFile.zip/kl_", Messstelle, "_hist_txt.zip")
	}else{
		downloadlink <- paste0("http://www.dwd.de/bvbw/generator/DWDWWW/Content/Oeffentlichkeit/KU/KU2/KU21/klimadaten/german/download/tageswerte/kl__", Messstelle, "__akt__txt,templateId=raw,property=publicationFile.zip/kl_", Messstelle, "_akt_txt.zip")
	}
	
	# Tempfile erzeugen
	zipfile <- tempfile("DWD_download_", fileext=".zip")
	
	# Zip-Datei runterladen
	download.file(downloadlink,zipfile)
	
	# Dateinamen aus der Zip-Datei auslesen
	files <- unzip(zipfile, exdir=tempdir())
	
	returnData <- NULL
	
	if(Metadaten){
		#---- Metadaten einlesen ----
		metaFile <- files[grepl(x=files, pattern="Stationsmetadaten_klima_stationen")]
		metaFileCon <- file(metaFile, encoding="ISO-8859-1")
		metadata <- read.table(metaFileCon, sep=";", header=TRUE, strip.white=TRUE, fill=TRUE)
		returnData$Metadaten <- cbind(Stationsname=metadata$Stationsname, Online_id=(rep(Messstelle, nrow(metadata))), metadata[,2:4], von=t(as.Date(as.character(metadata[,5]),format="%Y%m%d")), bis=t(as.Date(as.character(metadata[,6]),format="%Y%m%d")), stringsAsFactors=FALSE)
	}
	
	#---- Daten einlesen ----
	datafile <- files[grepl(x=files, pattern="produkt_klima_Tageswerte")]
	datafileCon <- file(datafile, encoding="ISO-8859-1")
	text <- gsub( ",", ";", readLines(datafileCon, warn=FALSE)) # erstmals beobachtet am 2014-05-01 haben die DWD-Daten nicht mehr "," als Trennzeichen, sondern ";". Allerdings sind manche Ueberschriften nich mit ",", daher diese Korrekturzeile
	close(datafileCon)
	
	returnData$Daten <- read.table(textConnection(text), sep=";", header=TRUE, strip.white=TRUE, na.strings=-999, as.is=TRUE, fill=TRUE)
	
	# letzte Zeile loeschen und Spalte loeschen
	returnData$Daten <- returnData$Daten[-nrow(returnData$Daten),-ncol(returnData$Daten)]
	
	# Alle Kopfzeilen in Großbuchstaben
	colnames(returnData$Daten) <- toupper(colnames(returnData$Daten))
	
	# Datumsspalte von character zu Datum umwandeln
	returnData$Daten$MESS_DATUM <- as.Date(as.character(returnData$Daten$MESS_DATUM), format="%Y%m%d")
	
	# Sich wiederholende character Strings in Faktoren umwandeln:
	returnData$Daten$STATIONS_ID <- as.factor(returnData$Daten$STATIONS_ID)
	
	if(Metadaten){
		#---- Fehldaten einlesen ----
		
		fehldaten <- files[grepl(x=files, pattern="Protokoll_Fehldaten_klima_stationen")]
		#returnData$Fehldaten <- readLines(fehldaten)
		
		
		#---- Zusatzinformationen einlesen ----
		
		weitereDaten <- files[c( -which(files==datafile), -which(files==metaFile),  -which(files==fehldaten))]
		
		returnData$Zusatzinfo <- NULL
		
		for(f in weitereDaten){
			addFile <- file(f, "r", encoding="ISO-8859-1")
			text <- gsub( "ISO-8859-1", "UTF-8", readLines(addFile))
			addData <- readHTMLTable(text, header=FALSE, stringsAsFactors = FALSE, which=1)
			Neu <- tail(addData, -2)
			colnames(Neu) <- addData[2,]
			rownames(Neu) <- NULL
			returnData$Zusatzinfo[[addData[1,1]]] <- Neu
			close(addFile)
		}
	}
	#---- Temp-Datei loeschen: ----
	unlink(c(zipfile,files))
	if(Metadaten){
		return(returnData)
	}else{
		return(returnData$Daten)
	}
}

updateDWDdata <- function(Datensatz){
	
	if(names(Datensatz[1])=="Metadaten"){
		Messstelle <- tail(Datensatz$Metadaten$Online_id,1)
	}else{
		if(names(Datensatz[1])=="STATIONS_ID"){
			stop("Der Datensatz kann nur aktualisiert werden, wenn er mit Metadaten heruntergeladen wurde")
		}else{
			stop("Der uebergebene Datensatz hat nicht (mehr) die Struktur eines DWD-Datensatzes")
		}	
	}
	
	
	aktuell <- getDWDdata(Messstelle, historisch=FALSE, Metadaten=FALSE)
	
	# historisch und aktuell zusammenfuegen
	
	# letztes Datum 1.Datensatz
	letztesDatum <- tail(Datensatz$Daten$MESS_DATUM, 1)
	
	# ergibt Zeilennummer von letztesDatum im aktuellen Datensatz
	AnfangAktuell <- which(letztesDatum==aktuell$MESS_DATUM)
	
	# wenn des Ende der bestehenden Daten vor dem Anfang der aktuellen Daten liegt, muessen noch Daten aus der historischen Datei hinzugefuegt werden:
	if(length(AnfangAktuell)==0) {
		historisch <- getDWDdata(Messstelle, historisch=TRUE, Metadaten=FALSE)
		
		# die historischen Daten muessen beginnen mit:
		AnfangHistorisch <- which(letztesDatum==historisch$MESS_DATUM)
		# mit den bestehenden Daten zusammenfuegen
		Datensatz$Daten <- rbind(Datensatz$Daten, historisch[(AnfangHistorisch+1):nrow(historisch), ])
		# neuer Anfang fuer die aktuellen Daten:
		AnfangAktuell <- which(tail(Datensatz$Daten$MESS_DATUM, 1) == aktuell$MESS_DATUM)
	}
	
	# verbindet 1. und 2. Datensatz
	Datensatz$Daten <- rbind(Datensatz$Daten,aktuell[(AnfangAktuell+1):nrow(aktuell) , ])
	
	return(Datensatz)
}
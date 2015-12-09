getDWDstations <- function(){
	#library("XML") # not needed in package
	DWDstationenURL <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/KL_Tageswerte_Beschreibung_Stationen.txt"
	stationFile <- tempfile()
	download.file(DWDstationenURL, stationFile)
	header <- read.table(stationFile, header = FALSE, sep = " ", stringsAsFactors = FALSE, nrows = 1)
	Stationen <- head(suppressWarnings(read.fwf(stationFile, widths = diff(c(0,12,21,30,45,57,67,108,201)), strip.white=TRUE, stringsAsFactors = FALSE, header=FALSE, skip=2, fileEncoding = "ISO-8859-1", encoding= "ISO-8859-1", col.names = header)), -1) # use head(, -1) to cut away the last row which is incomplete
	Stationen$Stations_id <- formatC(as.numeric(Stationen$Stations_id), width = 5, format = "d", flag = "0")
	return(Stationen)
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
		message("Get station ID by station name (to skip this part provide station ID directly)\n")
		# downloade Stationsliste wenn "Messstelle" ein character ist (Stationsname)
		stationen <- getDWDstations()
		Messstelle_nr <- stationen$Stations_id[which(stationen$Stationsname==Messstelle)]
		if(length(Messstelle_nr)==0) stop(paste0('Messstelle "', Messstelle, '" kann nicht gefunden werden'))
		message("\nStation ID is ", Messstelle_nr, "\n")
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
		message("\nmerge datasets\n")
		aktuell$Daten <- rbind(historisch,aktuell$Daten[(zl.nr+1):nrow(aktuell$Daten) , ])
		if(Metadaten){
			return(aktuell)
		}else{
			return(aktuell$Daten)
		}
	}
	
	if(historisch){
		message("\ndownload historical dataset\n")
		filenames <- strsplit(getURL("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/", ftp.use.epsv=FALSE, dirlistonly=TRUE), "\n")[[1]]
		ids <- substr(filenames, 12, 16)
		downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/", filenames[which(ids==Messstelle)])
	}else{
		message("\ndownload recent dataset\n")
		downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_", Messstelle, "_akt.zip")
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
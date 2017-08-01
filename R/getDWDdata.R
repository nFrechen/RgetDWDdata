getDWDstations <- function(){
	#library("XML") # not needed in package
	DWDstationenURL <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/KL_Tageswerte_Beschreibung_Stationen.txt"
	stationFile <- tempfile()
	dexit = 1
	try(dexit <- download.file(DWDstationenURL, stationFile))
	# Exit-Status des downloads überprüfen
	if(dexit!=0){
		message("Sorry, the station list could not be downloaded from ", DWDstationenURL)
		return()
	}
	
	col_names <- unlist(na.omit(read_table(stationFile, locale = locale(encoding = "windows-1252"), comment="-", n_max=1, col_names=F)), use.names = F)
	
	Stationen <- na.omit(read_table(stationFile, locale = locale(encoding = "windows-1252"), comment="-", skip=4, col_names=col_names))
	
	Stationen$Stations_id <- formatC(as.numeric(Stationen$Stations_id), width = 5, format = "d", flag = "0")
	Stationen$von_datum <- as.Date(as.character(Stationen$von_datum), format="%Y%m%d")
	Stationen$bis_datum <- as.Date(as.character(Stationen$bis_datum), format="%Y%m%d")
	return(Stationen)
}



getDWDdata <- function(Messstelle, historisch=FALSE, Metadaten=FALSE, Namen_ersetzen=TRUE){
	
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
		aktuell <- getDWDdata(Messstelle, historisch=FALSE, Metadaten = TRUE, Namen_ersetzen = Namen_ersetzen)
		historisch <- getDWDdata(Messstelle, historisch=TRUE, Metadaten = FALSE, Namen_ersetzen = Namen_ersetzen)
		
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
		filenames <- getURL("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/", ftp.use.epsv=FALSE, dirlistonly=TRUE)
		filenames <- gsub("\r\n", "\n", filenames)
		filenames <- strsplit(filenames, "\n")[[1]]
		ids <- substr(filenames, 15, 19)
		downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/", filenames[which(ids==Messstelle)])
	}else{
		message("\ndownload recent dataset\n")
		downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_", Messstelle, "_akt.zip")
	}
	
	# Tempfile erzeugen
	zipfile <- tempfile("DWD_download_", fileext=".zip")
	
	# Zip-Datei runterladen
	dexit = 1
	try(dexit <- download.file(downloadlink,zipfile))
	
	# Exit-Status des downloads überprüfen
	if(dexit!=0){
		message("The file at ", downloadlink, " could not be downloaded.")
		return()
	} 
	
	# Dateinamen aus der Zip-Datei auslesen
	files <- unzip(zipfile, exdir=tempdir())
	
	returnData <- NULL
	
	if(Metadaten | Namen_ersetzen){
		#---- Metadaten einlesen ----
		metaFile <- files[grepl(x=files, pattern="Metadaten_Geographie")]
		metaFileCon <- file(metaFile, encoding="ISO-8859-1")
		metadata <- read.table(metaFileCon, sep=";", header=TRUE, strip.white=TRUE, fill=TRUE)
		metadata$von_datum	= as.Date(as.character(metadata$von_datum),format="%Y%m%d")
		metadata$bis_datum	= as.Date(as.character(metadata$bis_datum),format="%Y%m%d")
		returnData$Metadaten <- metadata
	}
	
	#---- Daten einlesen ----
	datafile <- files[grepl(x=files, pattern="produkt_klima_tag_")]
	datafileCon <- file(datafile, encoding="ISO-8859-1")
	returnData$Daten <- read.table(datafileCon, sep=";", header=TRUE, strip.white=TRUE, na.strings=-999, as.is=TRUE, fill=TRUE)
	
	
	# letzte Spalte loeschen
	returnData$Daten <- returnData$Daten[,-ncol(returnData$Daten)]
	
	# Datumsspalte von character zu Datum umwandeln
	returnData$Daten$MESS_DATUM <- as.Date(as.character(returnData$Daten$MESS_DATUM), format="%Y%m%d")
	
	# Sich wiederholende character Strings in Faktoren umwandeln:
	returnData$Daten$STATIONS_ID <- as.factor(returnData$Daten$STATIONS_ID)
	
	if(Metadaten | Namen_ersetzen){

		#---- Zusatzinformationen einlesen ----
		
		textfiles <- files[grepl("txt", files)]
		weitereDaten <- textfiles[c( -which(textfiles==datafile), -which(textfiles==metaFile))]
		
		returnData$Zusatzinfo <- NULL
		
		for(i in seq_along(weitereDaten)){
			addFile <- file(weitereDaten[i], "r", encoding="ISO-8859-1")
			addData <- read.table(addFile, sep=";", header=TRUE, strip.white=TRUE, na.strings=-999, as.is=TRUE, fill=TRUE)
			addData <- tail(addData, -1)
			returnData$Zusatzinfo[[i]] <- addData
			names(returnData$Zusatzinfo)[i] <- gsub(".txt", "", basename(weitereDaten[i]))
			
			close(addFile)
		}
	}
	
	if(Namen_ersetzen){
		replacements <- head(unique(returnData$Zusatzinfo$Metadaten_Parameter_klima_tag[,5:7]),-1)
		ind_replacements <- na.omit(match(colnames(returnData$Daten), replacements$Parameter))
		ind_colnames <- na.omit(match(replacements$Parameter, colnames(returnData$Daten)))
		colnames(returnData$Daten)[ind_colnames] <- replacements$Parameterbeschreibung[ind_replacements]
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
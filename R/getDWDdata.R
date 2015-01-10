getDWDstations <- function(){
	#library("XML") # not needed in package
	DWDstationenURL <- "http://www.dwd.de/sid_gCpjSTGJDhT7rZvV38t3vSJWnnQc1HLyFcD46pL789crw0MpqGrg!295356740!-364271037!1385038953230/bvbw/appmanager/bvbw/dwdwwwDesktop?_nfpb=true&_pageLabel=dwdwww_result_page&portletMasterPortlet_i1gsbDocumentPath=Navigation%2FOeffentlichkeit%2FKlima__Umwelt%2FKlimadaten%2Fkldaten__kostenfrei%2Fstations_C3_BCbersicht__tabelle__node.html%3F__nnn%3Dtrue"
	
	stationen <- readHTMLTable(DWDstationenURL, stringsAsFactors = FALSE, which=7, skip.rows=1, header=TRUE)
	return(stationen)
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
		letztesDatum <- historisch$Mess_Datum[ nrow(historisch) ]
		
		# ergibt Zeilennummer von letztesDatum im aktuellen Datensatz
		zl.nr <- which(letztesDatum==aktuell$Daten$Mess_Datum)
		
		# verbindet 1. und 2. Datensatz
		aktuell$Daten <- rbind(historisch,aktuell$Daten[(zl.nr+1):nrow(aktuell$Daten) , ])
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
	
	# Datumsspalte von character zu Datum umwandeln
	returnData$Daten$Mess_Datum <- as.Date(as.character(returnData$Daten$Mess_Datum), format="%Y%m%d")
	
	# Sich wiederholende character Strings in Faktoren umwandeln:
	returnData$Daten$Stations_ID <- as.factor(returnData$Daten$Stations_ID)
	
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
		if(names(Datensatz[1])=="Stations_ID"){
			stop("Der Datensatz kann nur aktualisiert werden, wenn er mit Metadaten heruntergeladen wurde")
		}else{
			stop("Der uebergebene Datensatz hat nicht (mehr) die Struktur eines DWD-Datensatzes")
		}	
	}
	
	
	aktuell <- getDWDdata(Messstelle, historisch=FALSE, Metadaten=FALSE)
	
	# historisch und aktuell zusammenfuegen
	
	# letztes Datum 1.Datensatz
	letztesDatum <- tail(Datensatz$Daten$Mess_Datum, 1)
	
	# ergibt Zeilennummer von letztesDatum im aktuellen Datensatz
	AnfangAktuell <- which(letztesDatum==aktuell$Mess_Datum)
	
	# wenn des Ende der bestehenden Daten vor dem Anfang der aktuellen Daten liegt, muessen noch Daten aus der historischen Datei hinzugefuegt werden:
	if(length(AnfangAktuell)==0) {
		historisch <- getDWDdata(Messstelle, historisch=TRUE, Metadaten=FALSE)
		
		# die historischen Daten muessen beginnen mit:
		AnfangHistorisch <- which(letztesDatum==historisch$Mess_Datum)
		# mit den bestehenden Daten zusammenfuegen
		Datensatz$Daten <- rbind(Datensatz$Daten, historisch[(AnfangHistorisch+1):nrow(historisch), ])
		# neuer Anfang fuer die aktuellen Daten:
		AnfangAktuell <- which(tail(Datensatz$Daten$Mess_Datum, 1) == aktuell$Mess_Datum)
	}
	
	# verbindet 1. und 2. Datensatz
	Datensatz$Daten <- rbind(Datensatz$Daten,aktuell[(AnfangAktuell+1):nrow(aktuell) , ])
	
	return(Datensatz)
}
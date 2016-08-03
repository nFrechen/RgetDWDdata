list.ftp <- function(url, full.names=FALSE){
	filenames <- getURL(url, ftp.use.epsv=FALSE, dirlistonly=TRUE)
	#on windows \r\n has to be replaced by \n
	filenames <- gsub("\r\n", "\n", filenames)
	filenames <- strsplit(filenames, "\n")[[1]]
	if(full.names){
		return(file.path(url, filenames))
	}else{
		return(filenames)	
	}
	
}

#---------- Stationsnamen herunterladen ------------------
getDWDhourlyStations <- function(Parameter, historisch=F) {
	
	Obs<-data.frame(Parameter =c("sun", "air_temperature", "cloudiness", "precipitation", "pressure", "soil_temperature", "wind", "solar"),
									Dateiname=c("SD_Stundenwerte_Beschreibung_Stationen.txt", "TU_Stundenwerte_Beschreibung_Stationen.txt",
															"N_Stundenwerte_Beschreibung_Stationen.txt", "RR_Stundenwerte_Beschreibung_Stationen.txt",
															"P0_Stundenwerte_Beschreibung_Stationen.txt", "EB_Stundenwerte_Beschreibung_Stationen.txt",
															"FF_Stundenwerte_Beschreibung_Stationen.txt", "ST_Stundenwerte_Beschreibung_Stationen.txt"))
	
	txt<-as.vector(Obs[Obs$Parameter==paste(Parameter),2]) 
	
	colnames_stationen <- as.vector(t(read.table(url(paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", Parameter,
																													"/recent/",txt)), nrows = 1)) )
	
	stationsURL <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", Parameter, ifelse(historisch, "/historical/", "/recent/"),txt)
	
	
	text <- head(suppressWarnings(readLines(url(stationsURL, encoding="ISO-8859-1"))), -1)
	# read station id seperately since it is formatted differently
	# sometimes it is formattet "          3", sometimes "00003", so we have to reformat it:
	Stations_id <- gsub("^ *([0-9]*).*", "\\1", text)[c(-1,-2)]
	# convert it to common format like "00003"
	Stations_id <- formatC(as.integer(Stations_id), width = 5, flag = "0",  format = "d")
	# read other text in the file as if the station id in front wasn't there:
	otherText <- textConnection(gsub("^ *([0-9]*)\ ", "", text, perl=TRUE))
	stationen <- read.fwf(otherText, widths = c(8,9,15,12,10,42,23),  col.names = colnames_stationen[-1], skip=2, strip.white=T)
	
	#combine station id and other text again:
	stationen <- cbind(Stations_id, stationen)

	return(stationen)
}

#------------------ Dateien listen -------------
getDWDhourly <- function(Messstelle, historisch=F, Parameter, Metadaten=F){
	
	# Parameter-c("wind", "precipitation", "sun", "air_temperature", "pressure", "soil_temperature", "cloudiness")  
	
	#require("RCurl") # has to be loaded in the DESCRIPTION file
	
	#------------------- Dateien runterladen ---------
	
	#MesssttellenNR aus Namen ableiten
	if(is.na(suppressWarnings(as.numeric(Messstelle)))) {
		if(!is.na(historisch)){
			if(historisch){
				stationen<-getDWDhourlyStations(Parameter, historisch=F)
				Messstelle_nr<- stationen$Stations_id[which(stationen$Stationsname==Messstelle | stationen$Stations_id==Messstelle)]
				if(length(Messstelle_nr)==0) stop(paste0('Messstelle "', Messstelle, '" kann nicht im aktuellen Ordner ', Parameter , ' gefunden werden'))
				Messstelle <- formatC(as.integer(as.character(Messstelle_nr)), width = 5, flag = "0",  format = "d")
			}else{
				stationen<-getDWDhourlyStations(Parameter, historisch=T)
				Messstelle_nr<- stationen$Stations_id[which(stationen$Stationsname==Messstelle | stationen$Stations_id==Messstelle)]
				if(length(Messstelle_nr)==0) stop(paste0('Messstelle "', Messstelle, '" kann nicht im historischen Ordner ', Parameter , ' gefunden werden'))
				Messstelle <- formatC(as.integer(as.character(Messstelle_nr)), width = 5, flag = "0",  format = "d")
			}
		}
		
		if(is.na(historisch)){
			stationen_a<-getDWDhourlyStations(Parameter, historisch=F)
			Messstelle_nr_a<- stationen_a$Stations_id[which(stationen_a$Stationsname==Messstelle | stationen_a$Stations_id==Messstelle)]
			stationen_h<-getDWDhourlyStations(Parameter, historisch=T)
			Messstelle_nr_h<- stationen_h$Stations_id[which(stationen_h$Stationsname==Messstelle | stationen_h$Stations_id==Messstelle)]
			if(as.vector(Messstelle_nr_a)==as.vector(Messstelle_nr_h)){
				Messstelle<-formatC(as.integer(as.character(Messstelle_nr_a)), width = 5, flag = "0",  format = "d")
			}else{
				print("Messtelle",Messstelle,"ist nicht in beiden Datensätzen vorhanden")}
		}
	}
	
	#wenn MesstellenID bekannt und historisch TRUE or FALSE!    
	if(!is.na(historisch)){
		if(historisch){
			histoDat<-sort(list.ftp(url= paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", Parameter,"/historical/"), full.names = TRUE))
			histDataUrl <- histoDat[grep(Messstelle, histoDat)]
			}else{
			aktuDat <-list.ftp(paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", Parameter, "/recent/"), full.names=TRUE)
			aktDataUrl <- aktuDat[grep(Messstelle, aktuDat)]
		}    
	}  
	
	# kombiniert die beiden Datensätze, wenn historisch=NA:
	if(is.na(historisch)){
		
		aktuell <- try(getDWDhourly(Messstelle, historisch=F, Parameter, Metadaten=FALSE), silent=T)
		#aktuell <- try(getDWDhourly("Cottbus", historisch=F, "sun", Metadaten=FALSE), silent=T)
		
		if ('try-error' %in% class(aktuell)) stop(print("keine aktuellen Daten"))
		historisch <- try(getDWDhourly(Messstelle, historisch=T, Parameter, Metadaten = FALSE), silent)
		#historisch <- try(getDWDhourly("Cottbus", historisch=T, "sun", Metadaten = FALSE), silent)
		
		if ('try-error' %in% class(historisch)) stop(print("keine historischen Daten"))
		
		# historisch und aktuell zusammenfügen
		
		# letztes Datum 1.Datensatz
		letztesDatum <- historisch$MESS_DATUM[ length(historisch$MESS_DATUM) ]
		
		# ergibt Zeilennummer von letztesDatum im aktuellen Datensatz
		zl.nr <- which(aktuell$MESS_DATUM==letztesDatum)
		
		# verbindet 1. und 2. Datensatz
		if (all(names(aktuell) %in% names(historisch))==TRUE) {
			aktuell<- rbind(historisch, aktuell[(zl.nr+1):length(aktuell$MESS_DATUM),])
			return(aktuell)
		}else{ 
			col_names<-c(names(aktuell),names(historisch))
			col_names<-as.vector(col_names[duplicated(col_names)])
			aktuell <- subset(aktuell, select = col_names)
			historisch<-subset(historisch, select = col_names)
			aktuell<- rbind(historisch,aktuell[(zl.nr+1):length(aktuell$Mess_Datum) , ])
			return(aktuell)
		}
	}
	
	
	if(historisch==T){
		downloadlink <- histDataUrl
		}else{
		downloadlink <- aktDataUrl
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
	datafile <- files[grepl(x=files, pattern="produkt_")]
	datafileCon <- file(datafile)# encoding="ISO-8859-1")
	returnData$Daten <- read.csv(datafileCon, sep=";", strip.white=T, na.strings=-999, row.names=NULL, as.is=T, fill=T)
	
	
	# letzte Zeile löschen und Spalte löschen
	returnData$Daten <- returnData$Daten[-nrow(returnData$Daten),-ncol(returnData$Daten)]
	
	# Alle Kopfzeilen in Großbuchstaben
	colnames(returnData$Daten) <- toupper(colnames(returnData$Daten))
	
	# Datumsspalte von character zu Datum umwandeln
	returnData$Daten$MESS_DATUM <-as.POSIXct(as.character(returnData$Daten$MESS_DATUM), format="%Y%m%d%H", tz="GMT") 
	
	# Sich wiederholende character Strings in Faktoren umwandeln:
	returnData$Daten$STATIONS_ID <- as.factor(returnData$Daten$STATIONS_ID)
	
	
	if(Metadaten){
		#---- Zusatzinformationen einlesen ----
		
		weitereDaten <- files[c( -which(files==datafile), -which(files==metaFile) )] 
		
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


# some test cases:
if(FALSE){
	View(getDWDhourlyStations(Parameter = "air_temperature", historisch = TRUE))
	View(getDWDhourlyStations(Parameter = "air_temperature", historisch = FALSE))
	View(getDWDhourlyStations(Parameter = "precipitation", historisch = TRUE))
}

#------------------ Dateien listen -------------
getDWDhourly <- function(Messstelle, historisch=F, Parameter){
  
 possibleParameters <- c("wind", "precipitation", "sun", "air_temperature", "solar", "soil_temperature", "cloudiness")  
  
	if(!Parameter%in%possibleParameters) stop('Parameter must be one of "', paste(possibleParameters, collapse = '", "'), '"')
 
 require("RCurl")
aktuDat <-strsplit(getURL(paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", Parameter, "/recent/"), dirlistonly = TRUE), "\n")[[1]]
histoDat<-sort(strsplit(getURL(paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/",Parameter,"/historical/"), dirlistonly = TRUE), "\n")[[1]])

#View(aktuDat)
#View(histoDat)

aktuDatStID <-substr(aktuDat[], 17, 21)
histoDatStID<-substr(histoDat[], 17, 21)


#welche Datensätze sind sowohl historisch als auch aktuell verfügbar (swa~sowohlalsauch)
cDat<-c(aktuDatStID,histoDatStID)
swa<-duplicated(cDat)| duplicated(cDat,fromLast = TRUE)
#eoDat<-cDat[which(swa==F)]
swaDat<-cDat[which(swa==T)]


#------------------- Dateien runterladen ---------
if(is.na(suppressWarnings(as.numeric(Messstelle)))) {

  stationen<-getDWDstationsHourly()
    
  Messstelle_nr<- stationen$Stations_id[which(stationen$Stationsname==Messstelle | stationen$Stations_id==Messstelle)]
  if(length(Messstelle_nr)==0) stop(paste0('Messstelle "', Messstelle, '" kann nicht gefunden werden'))
  Messstelle <- Messstelle_nr
}

# kombiniert die beiden Datensätze, wenn historisch=NA:
if(is.na(historisch)){
  if(is.element(Messstelle, swaDat)==F){
    stop("keine 2 Datensätze vorhanden")}
  if(is.element(Messstelle, swaDat)==T){
    aktuell <- getDWDhourly(Messstelle, historisch=F, Parameter)
    historisch <- getDWDhourly(Messstelle, historisch=T, Parameter)
    
    # historisch und aktuell zusammenfügen
    
    # letztes Datum 1.Datensatz
    letztesDatum <- historisch$Daten$MESS_DATUM[ nrow(historisch$Daten) ]
    
    # ergibt Zeilennummer von letztesDatum im aktuellen Datensatz
    zl.nr <- which(letztesDatum==aktuell$Daten$MESS_DATUM)
    
    # verbindet 1. und 2. Datensatz
    if (length(names(aktuell$Daten))== length(names(historisch$Daten))){
    aktuell$Daten <- rbind(historisch$Daten,aktuell$Daten[(zl.nr+1):nrow(aktuell$Daten) , ])
    return(aktuell)
    }else{ col_names<-c(names(aktuell$Daten),names(historisch$Daten))
           col_names<-as.vector(col_names[duplicated(col_names)])
           aktuell$Daten <- subset(aktuell$Daten, select = col_names)
           historisch$Daten<-subset(historisch$Daten, select = col_names)
           aktuell$Daten <- rbind(historisch$Daten,aktuell$Daten[(zl.nr+1):nrow(aktuell$Daten) , ])
           return(aktuell)
    }
  }
}

# comment: this does not work if you give the wrong Messstelle number!!!
if(historisch==T){
  downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", Parameter, "/historical/",histoDat[which(histoDatStID==Messstelle)])
}else{
   downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/", Parameter, "/recent/",aktuDat[which(aktuDatStID==Messstelle)])
}

# Tempfile erzeugen
zipfile <- tempfile("DWD_download_", fileext=".zip")

# Zip-Datei runterladen
download.file(downloadlink,zipfile)

# Dateinamen aus der Zip-Datei auslesen
files <- unzip(zipfile, exdir=tempdir())

returnData <- NULL

#---- Daten einlesen ----
datafile <- files[grepl(x=files, pattern="produkt_")]
datafileCon <- file(datafile)# encoding="ISO-8859-1")

returnData$Daten <- read.csv(datafileCon, sep=";", strip.white=T, na.strings=-999, row.names=NULL, as.is=T, fill=T)

# letzte Zeile löschen und Spalte löschen
returnData$Daten <- returnData$Daten[-nrow(returnData$Daten),-ncol(returnData$Daten)]

# Datumsspalte von character zu Datum umwandeln
returnData$Daten$MESS_DATUM <- as.POSIXct(as.character(returnData$Daten$MESS_DATUM), format="%Y%m%d%H", tz="GMT") 
                                 
# Sich wiederholende character Strings in Faktoren umwandeln:
returnData$Daten$STATIONS_ID <- as.factor(returnData$Daten$STATIONS_ID)

#---- Temp-Datei löschen: ----
unlink(c(zipfile,files))
return(returnData)
}


if(F){
  Cb_sun<-getDWDhourly("Cottbus", historisch=NA, Parameter="sun")
  Cb_temp<-getDWDhourly("Cottbus", historisch=NA, Parameter="air_temperature")
}


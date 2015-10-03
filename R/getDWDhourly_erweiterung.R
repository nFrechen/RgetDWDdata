

#------------------ Dateien listen -------------
getDWDhourly <- function(Messstelle, historisch=F, Metadaten=T){
require("RCurl")
library("XML")

aktuDat <-strsplit(getURL("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/recent/", dirlistonly = TRUE), "\n")[[1]]
histoDat<-sort(strsplit(getURL("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/historical/", dirlistonly = TRUE), "\n")[[1]])

#View(aktuDat)
#View(histoDat)

aktuDatStID <-sort(substr(aktuDat[], 17, 21))
histoDatStID<-sort(substr(histoDat[], 17, 21))


#welche Datensätze sind sowohl historisch als auch aktuell verfügbar (swa~sowohlalsauch)
cDat<-c(aktuDatStID,histoDatStID)
swa<-duplicated(cDat,fromLast = TRUE)
eoDat<-cDat[which(swa==F)]
swaDat<-cDat[which(swa==T)]


#------------------- Dateien runterladen ---------


if(is.na(suppressWarnings(as.numeric(Messstelle)))) {

  stationen<-getDWDpubStatHourly()
    
  Messstelle_nr<- stationen$Stations_id[which(stationen$Stationsname==Messstelle | stationen$Stations_id==Messstelle)]
  if(length(Messstelle_nr)==0) stop(paste0('Messstelle ', Messstelle, ' kann nicht gefunden werden'))
  Messstelle <- Messstelle_nr
}


# kombiniert die beiden Datensätze, wenn historisch=NA:
if(is.na(historisch)){
  if(is.element(Messstelle, swaDat)==F){
    stop("keine 2 Datensätze vorhanden")}
  if(is.element(Messstelle, swaDat)==T){
    aktuell <- getDWDhourly(Messstelle, historisch=F)
    historisch <- getDWDhourly(Messstelle, historisch=T, Metadaten=F)
    
    # historisch und aktuell zusammenfügen
    
    # letztes Datum 1.Datensatz
    letztesDatum <- historisch$Daten$Mess_Datum[ nrow(historisch$Daten) ]
    
    if (letztesDatum+60*60==aktuell$Daten$Mess_Datum[1]){
    
      # verbindet 1. und 2. Datensatz
      aktuell$Daten <- rbind(historisch$Daten,aktuell$Daten)
      return(aktuell)
    
    }
  }
}

if(historisch){
  downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/historical/",histoDat[which(histoDatStID==Messstelle)])
}else{
   downloadlink <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/recent/stundenwerte_RR_", Messstelle, "_akt.zip")
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
  metaFile <- files[grepl(x=files, pattern="Stationsmetadaten")]
  metaFileCon <- file(metaFile, encoding="ISO-8859-1")
  metadata <- read.table(metaFileCon, sep=";", header=T, strip.white=T, fill=T)
  returnData$Metadaten <- cbind(Stationsname=metadata$Stationsname, Online_id=(rep(Messstelle, nrow(metadata))), metadata[,2:4], von=t(as.Date(as.character(metadata[,5]),format="%Y%m%d")), bis=t(as.Date(as.character(metadata[,6]),format="%Y%m%d")), stringsAsFactors=F)
}

#---- Daten einlesen ----
datafile <- files[grepl(x=files, pattern="produkt_synop_Terminwerte_")]
datafileCon <- file(datafile, encoding="ISO-8859-1")

returnData$Daten <- read.table(datafileCon, sep=";", header=T, strip.white=T, na.strings=-999, as.is=T, fill=T)

# letzte Zeile löschen und Spalte löschen
returnData$Daten <- returnData$Daten[-nrow(returnData$Daten),-ncol(returnData$Daten)]

# Datumsspalte von character zu Datum umwandeln
returnData$Daten$Mess_Datum <-as.POSIXct(as.character(returnData$Daten$Mess_Datum), format="%Y%m%d%H", tz="GMT") 
                                 
# Sich wiederholende character Strings in Faktoren umwandeln:
returnData$Daten$Stations_ID <- as.factor(returnData$Daten$Stations_ID)

if(Metadaten){
  #---- Fehldaten einlesen ----
  
  #fehldaten <- files[grepl(x=files, pattern="Protokoll_Fehldaten_klima_stationen")]
  #returnData$Fehldaten <- readLines(fehldaten)
  
  
  #---- Zusatzinformationen einlesen ----
  
  weitereDaten <- files[c( -which(files==datafile), -which(files==metaFile))] # -which(files==fehldaten)
  
  returnData$Zusatzinfo <- NULL
  
  for(f in weitereDaten){
    addFile <- file(f, "r", encoding="ISO-8859-1")
    text <- gsub( "ISO-8859-1", "UTF-8", readLines(addFile))
    addData <- readHTMLTable(text, header=F, stringsAsFactors = FALSE, which=1)
    Neu <- tail(addData, -2)
    colnames(Neu) <- addData[2,]
    rownames(Neu) <- NULL
    returnData$Zusatzinfo[[addData[1,1]]] <- Neu
    close(addFile)
  }
}

#---- Temp-Datei löschen: ----
unlink(c(zipfile,files))
return(returnData)
}


if(F){
  test<-getDWDhourly("Cottbus", historisch=NA)
  str(test)
  tail(test$Daten)
  which(test$Daten$NIEDERSCHLAGSHOEHE==36.8)
  test$Daten[163000:165623 ,]
  test$Daten$NIEDERSCHLAGSHOEHE[101116]
}


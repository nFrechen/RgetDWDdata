getDWDpublicStations <- function(){
  library("XML")
  DWDstationenURL <- "http://www.dwd.de/sid_gCpjSTGJDhT7rZvV38t3vSJWnnQc1HLyFcD46pL789crw0MpqGrg!295356740!-364271037!1385038953230/bvbw/appmanager/bvbw/dwdwwwDesktop?_nfpb=true&_pageLabel=dwdwww_result_page&portletMasterPortlet_i1gsbDocumentPath=Navigation%2FOeffentlichkeit%2FKlima__Umwelt%2FKlimadaten%2Fkldaten__kostenfrei%2Fstations_C3_BCbersicht__tabelle__node.html%3F__nnn%3Dtrue"

  stationen <- readHTMLTable(DWDstationenURL, stringsAsFactors = F, which=7, skip.rows=1, header=T)
  return(stationen)
}



getDWDdata <- function(Messstelle, historisch=F, Metadaten=F){

  # "Messstelle" muss entweder die 5-stellige Stations-Kennziffer sein oder der Stationsame als character
  # welche muss entwerder "historisch" oder "aktuell" sein
  # historisch=T läd die historischen Daten herunter (reichen für gewöhnlich vom Beginn der Messungen bis zum letzten Jahreswechsel)
  # historisch=F läd die aktuellen Daten vom letzten Jahreswechsel bis gestern herunter
  # historisch=NA kombiniert die beiden Datensätze

  library("XML")
  
  if(!is.na(historisch)){
	  if(historisch){
	  	Metadaten=F
	  }	
  }


  if(is.na(suppressWarnings(as.numeric(Messstelle)))) {
    # downloade Stationsliste wenn "Messstelle" ein character ist (Stationsname)
    stationen <- getDWDpublicStations()
    Messstelle_nr <- stationen$`Stations-Kennziffer`[which(stationen$Stationsname==Messstelle | stationen$`Stations-Kennziffer`==Messstelle)]
    if(length(Messstelle_nr)==0) stop(paste0('Messstelle "', Messstelle, '" kann nicht gefunden werden'))
    Messstelle <- Messstelle_nr
  }
  # Ansonsten einfach die Nummer verwenden

  historisch <- as.logical(historisch)

  # kombiniert die beiden Datensätze, wenn historisch=NA:
  if(is.na(historisch)){
    aktuell <- getDWDdata(Messstelle, historisch=F, Metadaten = T)
    historisch <- getDWDdata(Messstelle, historisch=T, Metadaten = F)

    # historisch und aktuell zusammenfügen

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
    metadata <- read.table(metaFileCon, sep=";", header=T, strip.white=T, fill=T)
    returnData$Metadaten <- cbind(Stationsname=metadata$Stationsname, Online_id=(rep(Messstelle, nrow(metadata))), metadata[,2:4], von=t(as.Date(as.character(metadata[,5]),format="%Y%m%d")), bis=t(as.Date(as.character(metadata[,6]),format="%Y%m%d")), stringsAsFactors=F)
  }

  #---- Daten einlesen ----
  datafile <- files[grepl(x=files, pattern="produkt_klima_Tageswerte")]
  datafileCon <- file(datafile, encoding="ISO-8859-1")
  text <- gsub( ",", ";", readLines(datafileCon, warn=F)) # erstmals beobachtet am 2014-05-01 haben die DWD-Daten nicht mehr "," als Trennzeichen, sondern ";". Allerdings sind manche Überschriften nich mit ",", daher diese Korrekturzeile
  close(datafileCon)

  returnData$Daten <- read.table(textConnection(text), sep=";", header=T, strip.white=T, na.strings=-999, as.is=T, fill=T)

  # letzte Zeile löschen und Spalte löschen
  returnData$Daten <- returnData$Daten[-nrow(returnData$Daten),-ncol(returnData$Daten)]

  # Datumsspalte von character zu Datum umwandeln
  returnData$Daten$Mess_Datum <- as.Date(as.character(returnData$Daten$Mess_Datum), format="%Y%m%d")

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
  if(Metadaten){
  	return(returnData)
  }else{
  	return(returnData$Daten)
  }
}


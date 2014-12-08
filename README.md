RgetDWDdata
===========

R function to download daily climate data from the DWD (german weather service)

R Funktion zum herunterladen von Tageswerten der Klimadaten des DWD (Deutscher Wetter Dienst)



*************

## Benutzung

Die Funktion `getDWDdata()` läd die Klimadaten von den Servern des DWD herunter.

Die Argumente sind:

* `Messtelle =`, hier kann der Name der Messstelle angegeben werden oder die Stations-ID. Die möglichen Stationsnamen können [hier][1] eingesehen werden oder mit der Funktion `getDWDpublicStations()` in R geladen werden.
  
* `historisch =`
    + wenn `FALSE` wird der aktuelle Datensatz heruntergeladen (für gewöhnlich die letzten 360 Tageswerte, täglich aktualisiert)
    + wenn `TRUE` wird der historische Datensatz (ohne die aktuellen Daten) heruntergeladen
    + wenn `NA` (standard) werden historischer und aktueller Datensatz heruntergeladen und zusammen gefügt

* `Metadaten =` (logical, default=FALSE), definiert, ob auch Metadaten herunter geladen werden sollen (z.B. Koordinaten der Station); Achtung!: Die Struktur der Rückgabe ändert sich durch diese Option

--------

## Hintergrund

Die Daten werden von diesen beiden URLs heruntergeladen:
```r
if(historisch){
  downloadlink <- paste0("http://www.dwd.de/bvbw/generator/DWDWWW/Content/Oeffentlichkeit/KU/KU2/KU21/klimadaten/german/download/tageswerte/kl__", Messstelle, "__hist__txt,templateId=raw,property=publicationFile.zip/kl_", Messstelle, "_hist_txt.zip")
}else{
    downloadlink <- paste0("http://www.dwd.de/bvbw/generator/DWDWWW/Content/Oeffentlichkeit/KU/KU2/KU21/klimadaten/german/download/tageswerte/kl__", Messstelle, "__akt__txt,templateId=raw,property=publicationFile.zip/kl_", Messstelle, "_akt_txt.zip")
}
```

[1]: http://www.dwd.de/sid_gCpjSTGJDhT7rZvV38t3vSJWnnQc1HLyFcD46pL789crw0MpqGrg!295356740!-364271037!1385038953230/bvbw/appmanager/bvbw/dwdwwwDesktop?_nfpb=true&_pageLabel=dwdwww_result_page&portletMasterPortlet_i1gsbDocumentPath=Navigation%2FOeffentlichkeit%2FKlima__Umwelt%2FKlimadaten%2Fkldaten__kostenfrei%2Fstations_C3_BCbersicht__tabelle__node.html%3F__nnn%3Dtrue
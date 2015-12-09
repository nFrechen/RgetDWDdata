RgetDWDdata
===========

R function to download daily climate data from the DWD (German Weather Service)

R Funktion zum herunterladen von Tageswerten der Klimadaten des DWD (Deutscher Wetter Dienst)

*************
## Installation
In order to install from github you need the ```devtools``` package:
```R
install.packages("devtools")
```
With this you can install the ```RgetDWDdata``` package:
```R
library(devtools)
install_github("nFrechen/RgetDWDdata")
```



*************

## Benutzung

Die Funktion `getDWDdata()` lädt die Klimadaten von den Servern des DWD herunter.

Die Argumente sind:

* `Messtelle =`, hier kann der Name der Messstelle angegeben werden oder die Stations-ID. Die möglichen Stationsnamen können mit der Funktion `getDWDpublicStations()` geladen und angezeigt werden.
  
* `historisch =`
    + wenn `FALSE` wird der aktuelle Datensatz heruntergeladen (für gewöhnlich die letzten 360 Tageswerte, täglich aktualisiert)
    + wenn `TRUE` wird der historische Datensatz (ohne die aktuellen Daten) heruntergeladen
    + wenn `NA` (standard) werden historischer und aktueller Datensatz heruntergeladen und zusammen gefügt

* `Metadaten =` (logical, default=FALSE), definiert, ob auch Metadaten herunter geladen werden sollen (z.B. Koordinaten der Station); Achtung!: Die Struktur der Rückgabe ändert sich durch diese Option

--------


[1]: http://www.dwd.de/sid_gCpjSTGJDhT7rZvV38t3vSJWnnQc1HLyFcD46pL789crw0MpqGrg!295356740!-364271037!1385038953230/bvbw/appmanager/bvbw/dwdwwwDesktop?_nfpb=true&_pageLabel=dwdwww_result_page&portletMasterPortlet_i1gsbDocumentPath=Navigation%2FOeffentlichkeit%2FKlima__Umwelt%2FKlimadaten%2Fkldaten__kostenfrei%2Fstations_C3_BCbersicht__tabelle__node.html%3F__nnn%3Dtrue
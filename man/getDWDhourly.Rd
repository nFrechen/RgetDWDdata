\name{getDWDhourly}
\alias{getDWDhourly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Downloads hourly climate data from the German Weather Service (DWD)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getDWDhourly(Messstelle, historisch=F, Parameter)
}
\arguments{
  \item{Messstelle}{
	Name der Messstelle oder Stations-Kennziffer, wie sie in \code{\link{getDWDpubStatHourly}} zu finden sind.
}
\item{historisch}{
	Wenn TRUE wird der historische Datensatz heruntergeladen (geht für gewöhnlich bis zum 31. Dezember des Vorjahres), wenn FALSE wird der aktuelle Datensatz (für gewöhnlich die letzten 365 Tageswerte) heruntergeladen. NA lädt beide Datensätze herunter und fügt sie zusammen. Damit ergibt sich ein lückenloser Datensatz vom Beginn der Messung bis ein bis zwei Tage vor dem aktuellen Datum.
}
\item{Parameter}{
	one of the following parameters: "wind", "precipitation", "sun", "air_temperature", "solar", "soil_temperature", "cloudiness"
	}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Data source is: \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly}{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly} 
}
\author{
Tobias Recke
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For daily data use \code{\link{getDWDdata}}
}
\examples{
precipitation <- getDWDhourly("Cottbus", historisch = F, 
	Parameter = "precipitation")
plot(NIEDERSCHLAGSHOEHE~MESS_DATUM, precipitation$Daten, type="l")

}


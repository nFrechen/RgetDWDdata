\name{updateDWDdata}
\alias{updateDWDdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Aktualisiert einen DWD (Deutscher Wetterdienst) Datensatz
}
\description{
Um Downloadzeiten zu minimieren kann diese Funktion einen bereits heruntergeladenen Datensatz mit DWD-Daten aktualisieren. Sind die Daten die Daten weniger als 365 Tage alt, muss dafür nur der aktuelle Datensatz und nicht der (wesentlich größere) historische heruntergeladen werden. Die Funktion ermittelt automatisch die Stations-Kennung aus dem gegebenen Datensatz, dies funktioniert jedoch nur, wenn der Datensatz die original Metadaten enthält (also mit \code{Metadaten=TRUE} heruntergeladen wurde).
}
\usage{
updateDWDdata(Datensatz)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Datensatz}{
	Datensatz der aktualisiert werden soll.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\author{
Tobias Recke
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getDWDstations}}, \code{\link{getDWDdata}}
}
\examples{
# Lade historischen Datensatz:
Klimadaten <- getDWDdata("Cottbus", historisch=TRUE, Metadaten=TRUE)
tail(Klimadaten$Daten[,1:4])

# Aktualisieren historischen Datensatz mit aktuellen Daten:
Klimadaten <- updateDWDdata(Klimadaten)
tail(Klimadaten$Daten[,1:4])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

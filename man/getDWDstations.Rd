\name{getDWDstations}
\alias{getDWDstations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lädt die Liste verfügbarer Wetterstationen des DWD herunter 
}
\description{
Die Liste enthält Stationsnamen, Stations-Kennziffer, Stationshöhe, geografische Breite und Länge und den Beginn der Klimareihe.
}
\usage{
getDWDstations()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
Braucht keine Argumente
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
\references{
Quelle der Daten: \url{http://www.dwd.de}
}
\author{
Nanu Frechen
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getDWDdata}}, \code{\link{updateDWDdata}}
}
\examples{
getDWDstations()
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
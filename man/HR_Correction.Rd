\name{HR_Correction}
\alias{HR_Correction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HR_Correction(Area = NULL, Mass = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Area}{
%%     ~~Describe \code{Area} here~~
}
  \item{Mass}{
%%     ~~Describe \code{Mass} here~~
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Area = NULL, Mass = NULL) 
{
    ML_Correction_Factor <- function(x) {
        1/(1 + exp(0.3644301 * (x - 3.693227)))
    }
    Min_Correction_Factor <- function(x) {
        1/(1 + exp(0.1888994 * (x - 7.1498172)))
    }
    Max_Correction_Factor <- function(x) {
        1/(1 + exp(0.9177596 * (x - 0.2366375)))
    }
    ML_CF <- ML_Correction_Factor(log10(Mass))
    Max_CF <- Max_Correction_Factor(log10(Mass))
    Min_CF <- Min_Correction_Factor(log10(Mass))
    if (missing(Area)) {
        Corrected_Area <- round(c(1/Min_CF, 1/ML_CF, 1/Max_CF), 
            3)
        names(Corrected_Area) <- c("Min", "ML", "Max")
        Corrected_Area <- t(as.data.frame(Corrected_Area))
        row.names(Corrected_Area) <- NULL
        Corrected_Area
    }
    else {
        Corrected_Area <- c(Area, Mass, round(c(Area/Min_CF, 
            Area/ML_CF, Area/Max_CF), 3))
        names(Corrected_Area) <- c("Original Area", "Mass (kg)", 
            "Corrected Area - Min", "Corrected Area - ML", "Corrected Area - Max")
        Corrected_Area <- t(as.data.frame(Corrected_Area))
        row.names(Corrected_Area) <- NULL
        Corrected_Area
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

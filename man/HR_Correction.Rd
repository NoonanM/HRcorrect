\name{HR_Correction}
\alias{HR_Correction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Home range correction function
}
\description{
Correct a 95\% home range area estimates for the mass-specific bias in conventional estimators. The correction is based on the regressional analyses in Noonan et al. (2019) Body size dependent underestimation of home range areas. In prep.
}
\usage{
HR_Correction(Area = NULL, Mass = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Area}{
A home range area (units are preserved)
}
  \item{Mass}{
A mass in kg
}
}
\details{
Returns a 95\% home range area estimate corrected for the mass-specific bias in conventional estimators. If no area estimate is provided, the function instead returns a correction factor

}
\value{

A matrix containing the original area estimate, the mass, and the corrected home range area +/- 95\% confidence intervals.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Michael J. Noonan
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#Supply the function with an area estimate and a mass in kg
HR_Correction(Area = 15, Mass = 10)


#If no area estimate is provided, the function instead returns a correction factor
HR_Correction(Mass = 10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

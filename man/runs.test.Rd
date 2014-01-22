\name{runs.test}
\alias{runs.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Runs Test
}
\description{
Performs the non parametric Runs test of randomness for continuous data.
}
\usage{
runs.test(x, alternative, level, pvalue)
}
\arguments{
  \item{x}{a numeric vector containing the observations}
  \item{alternative}{the alternative hypothesis, must be one of "\code{two.sided}" (default), "\code{left.sided}" or "\code{right.sided}".}
  \item{level}{the cut-point to transform the data into a dichotomous vector}
  \item{pvalue}{a character string specifying the method used to compute the p-value. Must be one of normal (default), or exact.}  
}
\details{
Data is transformed into a dichotomous vector according as each values is above or below a given level. Values equal to the level are removed from the sample. 

The default \code{level} value used in applications is the sample median which give us the special case of this test with \eqn{n_1=n_2}, the runs test above and below the median.

The possible \code{alternative} values are "\code{two.sided}", "\code{left.sided}" and "\code{right.sided}" define the alternative hypothesis. By using the alternative "\code{left.sided}" the null of randomness is tested against a trend. By using the alternative "\code{right.sided}" the null hypothesis of randomness is tested against a first order negative autocorrelation.
}
\value{
A list with class "htest" containing the components:
\item{statistic}{the value of the statistic R, which counts the total number of runs, the theoretical mean value and variance of R.}
\item{p.value}{the p-value of the test.}
\item{method}{a character string indicating the test performed.}
\item{data.name}{a character string giving the name of the data.}
\item{parameter}{a vector with the sample size, and the values of \eqn{n_1} and \eqn{n_2}.}
}
\references{
Brownlee, K. A. (1965). \emph{Statistical Theory and Methodology in Science and Engineering}, 2nd ed. New York: Wiley.

Gibbons, J.D. and Chakraborti, S. (2003). \emph{Nonparametric Statistical Inference}, Fourth Edition (p. 78-86).
URL: \url{http://books.google.pt/books?id=dPhtioXwI9cC&lpg=PA97&ots=ZGaQCmuEUq}

Wald, A. and Wolfowitz, J. (1940). On a test whether two samples are from the same population, \emph{Ann. Math Statist.} 11, 147-162.
}
\author{Frederico Caeiro}
\examples{
##
## Data from example in Brownlee (1965), p. 223.
##
earthden <- c(5.36, 5.29, 5.58, 5.65, 5.57, 5.53, 5.62, 5.29, 5.44, 5.34, 5.79, 
5.10, 5.27, 5.39, 5.42, 5.47, 5.63, 5.34, 5.46, 5.30, 5.75, 5.68, 5.85)
runs.test(earthden)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ runs test }
\keyword{ randomness test }% __ONLY ONE__ keyword per line

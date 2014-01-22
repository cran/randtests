\name{difference.sign.test}
\alias{difference.sign.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Difference Sign Test}
%%  ~~function to do ... ~~

\description{
Performs the non parametric Difference Sign Test of randomness.
}
\usage{
difference.sign.test(x, alternative)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector containing the data}
  \item{alternative}{a character string specifying the alternative hypothesis. Must be one of "\code{two.sided}" (default), "\code{left.sided}" or "\code{right.sided}".}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list with class "htest" containing the components:
\item{statistic}{the (normalized) value of the statistic test.}
\item{n}{The size of the data, after the remotion of consecutive duplicate values.}
\item{p.value}{the p-value of the test.}
\item{method}{a character string indicating the test performed.}
\item{data.name}{a character string giving the name of the data.}
}
\references{
Brockwell, P.J., and Davis, R.A. (2002). \emph{Introduction to Time Series and Forecasting}, 2nd edition, Springer (p. 37).

Mateus, A. and Caeiro, F. (2013). Comparing several tests of randomness based on the difference of observations. In T. Simos, G. Psihoyios and Ch. Tsitouras (eds.), \emph{AIP Conf. Proc.} \bold{1558}, 809-812.

Moore, G. H. and Wallis, W. A. (1943). Time Series Significance
Tests Based on Signs of Differences, \emph{Journal of the
American Statistical Association}, \bold{38}, 153-154.
%% ~put references to the literature/web site here ~
}
\author{
Ayana Mateus and Frederico Caeiro
}
\examples{
##
## Example 1
## Annual Canadian Lynx trappings 1821-1934 in Canada.
## Available in datasets package
##
\dontrun{plot(lynx)}
difference.sign.test(lynx)

##
## Example 2
## Sweet potato production, harvested in the United States, between 1868 and 1937.
##
data(sweetpotato)
difference.sign.test(sweetpotato$production)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ difference sign test }
\keyword{ randomness test }% __ONLY ONE__ keyword per line

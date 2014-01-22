\name{rank.test}
\alias{rank.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mann-Kendall Rank Test
}
\description{
Performs the non parametric Mann-Kendall Rank Test of randomness.
}
\usage{
rank.test(x, alternative)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector containing the observations}
  \item{alternative}{a character string specifying the alternative hypothesis. Must be one of "\code{two.sided}" (default), "\code{left.sided}" or "\code{right.sided}".}
  downward trend:
}
\details{
Missing values are removed.

The possible \code{alternative} values are "\code{two.sided}", "\code{left.sided}" and "\code{right.sided}" define the alternative hypothesis. By using the alternative "\code{left.sided}" the null of randomness is tested against a downward trend. By using the alternative "\code{right.sided}" the null hypothesis of randomness is tested against a rising trend.
}
\value{
A list with class "htest" containing the components:
\item{statistic}{the value of the statistic test.}
\item{n}{The size of the data.}
\item{p.value}{the p-value of the test.}
\item{method}{a character string indicating the test performed.}
\item{data.name}{a character string giving the name of the data.}
}
\references{
Brockwell, P.J. and Davis, R.A. (2002). \emph{Introduction to Time Series and Forecasting}, 2nd edition, Springer (p. 37).

Mann, H.B. (1945). Nonparametric test against trend. \emph{Econometrica}, \bold{13}, 245-259.

Kendall M. (1990). Rank correlation methods, 5th edition. Oxford University Press, USA.
}
\author{Ayana Mateus and Frederico Caeiro}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ rank test }
\keyword{ randomness test }% __ONLY ONE__ keyword per line

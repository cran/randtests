\name{bartels.rank.test}
\alias{bartels.rank.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bartels Rank Test
}
\description{
Performs the non parametric Bartels rank test of randomness.
}
\usage{
bartels.rank.test(x, alternative, pvalue)
}
% alternative = c("two.sided", "less", "greater")
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector containing the observations}
  \item{alternative}{a character string specifying the alternative hypothesis. Must be one of "\code{two.sided}" (default), "\code{left.sided}" or "\code{right.sided}".}
  \item{pvalue}{a character string specifying the method used to compute the p-value. Must be one of \code{normal} (default), \code{beta} or \code{auto}.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Missing values are removed.

The possible \code{alternative} values are "\code{two.sided}", "\code{left.sided}" and "\code{right.sided}" define the alternative hypothesis. By using the alternative "\code{left.sided}" the null of randomness is tested against a trend. By using the alternative "\code{right.sided}" the null hypothesis of randomness is tested against a systematic oscillation.
}
\value{
A list with class "htest" containing the components:
\item{statistic}{the value of the RVN statistic test and the theoretical mean value and variance of the RVN statistic test.}
\item{n}{The size of the data, after the remotion of consecutive duplicate values.}
\item{p.value}{the p-value of the test.}
\item{method}{a character string indicating the test performed.}
\item{data.name}{a character string giving the name of the data.}
\item{alternative}{a character string describing the alternative.}
\item{parameter}{The size of the data.}
}
\references{
Bartels, R. (1982). The Rank Version of von Neumann's Ratio Test for Randomness, \emph{Journal of the American Statistical Association}, \bold{77}(377), 40-46.

Gibbons, J.D. and Chakraborti, S. (2003). \emph{Nonparametric Statistical Inference}, Fourth Edition (pp. 97-98).
URL: \url{http://books.google.pt/books?id=dPhtioXwI9cC&lpg=PA97&ots=ZGaQCmuEUq}
}
\author{Frederico Caeiro}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rank.test}}
}
\examples{
##
## Example 5.1 in Gibbons and Chakraborti (2003), p.98.
##
years<-1970:1982
tourists<-c(12362, 12739, 13057, 13955, 14123,  15698, 17523, 
18610, 19842, 20310, 22500, 23080, 21916)
plot(years, tourists)
bartels.rank.test(tourists, alternative="left.sided")

##
## Example in Bartels (1982).
##
GPDprice<-c(83.9, 86.3, 87.3, 88.4, 91.6, 94, 96.9, 100, 102.6, 106.2, 110.9, 117.2, 
          125.5, 136.3, 155.4, 184.5, 211.8, 234.9, 254.1)
income<-c(420, 361, 338, 463, 558, 613, 513, 530, 601, 747, 801, 790, 766, 817, 603, 
          155, 243, 829, 1255)
x <- income/GPDprice * 100
bartels.rank.test(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ bartels rank test }
\keyword{ randomness test }% __ONLY ONE__ keyword per line

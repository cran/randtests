\name{randtests-package}
\alias{randomtests-package}
\docType{package}
\title{
Testing randomness in R
}
\description{
The package \code{randtests} implements several nonparametric randomness tests of hypothesis. 
}
\details{
\tabular{ll}{
Package: \tab randomtests\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2014-01-16\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}
Randomness is a common assumption in many statistical methods. When such assumption is not fulfilled, researchers may draw wrong conclusions. In many applications, a simple graphical analysis is enough to check such assumption. But there are cases where a test of hypothesis is required.
Since no single test is likely to be efficient against all types of nonrandomness, in this package we implemented several nonparametric randomness tests. We have included in the package the most common tests available in the literature, based on the sign of the difference of observations, ranks or runs. 
%For a power comparison between randomness tests based on the sign of  the difference of  observations see \cite{mateus2013}.
}
\author{
Frederico Caeiro and Ayana Mateus

Maintainer: Frederico Caeiro <fac@fct.unl.pt>
}

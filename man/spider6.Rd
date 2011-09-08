\name{spider6}
\alias{spider6}
\docType{data}
\title{The spider data set}
\description{
Data set on abundances of spiders and environmental predictors.
This is a subset of a larger data comprising 12 species and 6 environmental predictors.
All variables are rated on a 0-9 scale.
}
\usage{data(spider6)}
\format{
  A data frame with 28 observations on the following 9 variables.
  \describe{
    \item{\code{Pard.lugu}}{a numeric vector}
    \item{\code{Pard.pull}}{a numeric vector}
    \item{\code{Troc.terr}}{a numeric vector}
    \item{\code{Pard.mont}}{a numeric vector}
    \item{\code{Alop.acce}}{a numeric vector}
    \item{\code{Alop.fabr}}{a numeric vector}
    \item{\code{Water}}{a numeric vector}
    \item{\code{Herbs}}{a numeric vector}
    \item{\code{Site}}{a factor with 28 levels}
  }
}
\source{
package mvpart
}
\references{
De'ath G. (2002) \emph{Multivariate regression trees: A new technique for modeling
species-environment relationships}. Ecology, 2002, 83:1105--1117.
}
\examples{
data(spider6)
summary(spider6)
fit0 <- mdm(y2p(spider6[,1:6])~1,data=spider6)
fit1 <- mdm(y2p(spider6[,1:6])~Water+Herbs,data=spider6)
fit2 <- mdm(y2p(spider6[,1:6])~Site,data=spider6,alpha=TRUE)
anova(fit0,fit1,fit2)
}
\keyword{datasets}

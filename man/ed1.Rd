\name{ed1}
\alias{ed1}
\title{Calculate alpha, beta and gamma true entropies and diversities}
\description{
Calculates true diversities of individual sites of a data set}
\usage{
ed1(x, q = 1, retq = TRUE)
}
\arguments{
  \item{x}{
the input matrix or data frame.
}
  \item{q}{
the order of diversity; typically 0, 1 or 2.
}
  \item{retq}{
if TRUE then diversities are returned; if FALSE the entropies for alpha
and gamma are returned.
}
}
\value{
a vector of entropies or diversities
}
\seealso{
\code{\link{dev2div}}, \code{\link{ed1}}, \code{\link{eds}}, \code{\link{eds1}}
}
\examples{
data(spider6)
ed1(spider6[,1:6])
ed1(spider6[,1:6],q=0)
ed1(spider6[,1:6],q=2)
ed1(spider6[,1:6],retq=FALSE)
data(spider6)
ed1(spider6[,1:6])
}

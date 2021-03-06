\name{eds}
\alias{eds}
\title{Calculate alpha, beta and gamma parametric entropies and diversities}
\description{
Calculates alpha, beta and gamma parametric diversities of a data set.
The matrix or data frame is automatically scaled to row sums of one.
Unlike true diversities where weighting is done through the function ed,
weighting for parameterized diversity is done within the MDM,
or more genrelaly by using case weights.}
\usage{
eds(x, q = 1, w = 1, retq = TRUE)
}
\arguments{
  \item{x}{
the input matrix or data frame.
}
  \item{w}{
weights if required.
}
  \item{q}{
the order of diversity; typically 0, 1 or 2.
}
  \item{retq}{
if TRUE then parametric diversities are returned; if FALSE the entropies for alpha and gamma are returned.
}
}
\value{
a vector of entropies or diversities
}
\seealso{
\code{\link{dev2div}}, \code{\link{ed1}}, \code{\link{ed}}, \code{\link{eds1}}
}
\examples{
data(spider6)
eds(spider6[,1:6])
eds(spider6[,1:6],q=0)
eds(spider6[,1:6],q=2)
eds(spider6[,1:6],retq=FALSE)
data(spider6)
eds(spider6[,1:6])
}

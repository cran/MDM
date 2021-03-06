\name{simdata}
\alias{simdata}
\title{Species abundance data simulator}
\description{
Simulates species abundance data along a one-dimensional gradient
}
\usage{
simdata(d, p = 10, n = 100, strip0 = TRUE, extreme = 0,
ret = TRUE, k.rand = FALSE, d.rand = TRUE, mu.rand = TRUE,
s = rep((4:8)/10, length = p), amp = c(sample(1:5), rep = TRUE,
length = p), skew = 1, ampfun, lst = FALSE, err = 1,
err.type = c("p", "n")[1], as.df = TRUE, plotit = TRUE,
ptype = "l", plty = 1, pcols = rainbow(p), add.rug = FALSE, ...)
}
\arguments{
  \item{d}{
the (optional) locations of the species along the 1-D gradient. If d is given
then it will define both the number of species and also the locations on the gradient
e.g. d = rep(1:10,each=3) will generate species at locations 1,1,1,2,2,2,...,10,10,10.
If d is not specified then d.rand = TRUE will randomly allocate the species modes
along a gradient on [0, 1], but if d.rand = FALSE will uniformly distribute
the species modes along a gradient.
}
  \item{p}{
number of species.
}
  \item{n}{
number of sites.
}
  \item{strip0}{
if TRUE the sites with zero total abundance are omitted.
}
  \item{extreme}{
number typically in the range -1 to +1 with larger numbers reducing the range of species.
}
  \item{ret}{
if TRUE the generated data are returned
}
  \item{k.rand}{
should the be random (TRUE) or fixed
}
  \item{d.rand}{
should the be random (TRUE) or fixed
}
  \item{mu.rand}{
should the be random (TRUE) or fixed
}
  \item{s}{
the spans of the species response curves; s is the standard deviation of the spread
}
  \item{amp}{
the amplitudes of the species response curves
}
  \item{skew}{
the skewness of the distribution; range (>0 to 5), 1 = symmetric.
}
  \item{ampfun}{
any function to modify the amplitude
}
  \item{lst}{
if lst == TRUE then both the systematic and random values are returned
}
  \item{err}{
if err == 0 then the values are systematic with no random variation
}
  \item{err.type}{
type of error; p = poison, g = gaussian
}
  \item{as.df}{
if return returns a data frame, otherwise a matrix
}
  \item{plotit}{
if TRUE then the data are plotted
}
  \item{ptype}{
species plot types e.g. "l" gives lines
}
  \item{plty}{
species plot line types
}
  \item{pcols}{
species plot colours
}
  \item{add.rug}{
should a rug be added?
}
  \item{\dots}{
other arguments passed to plot.
}
}
\value{
if lst == FALSE then a data frame with variables "Locations", "Taxa.1" -- "Taxa.N"
where N is number of species.
if lst == TRUE then two data frames "x" and "xs" with variables "Locations",
"Taxa.1" -- "Taxa.N" and additionally, components "sigma", "amp" and "mu" that
represent the spans, amplitudes and locations of the N species along the 1-D gradient.
}
\examples{
mydata <- simdata()
summary(mydata)
mydata <- simdata(p=5, n=50, amp=1, err=0, d.rand=FALSE,
mu.rand=FALSE, plotit = TRUE)
summary(mydata)
}

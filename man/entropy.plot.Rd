\name{entropy.plot}
\alias{entropy.plot}
\title{Entropy plot}
\description{
Plots components of entropy by species or sites.
}
\usage{
entropy.plot(lst, y, x, ord=TRUE, type=c("species","sites")[1],
 labs, segs = TRUE, wid.seg=0.25,
 pchs = c(15,19,0,2,6,2,15,17,21,17), ...)
}
\arguments{
  \item{lst}{
a list of \code{\link{mdm}} fitted models
}
  \item{y}{
optional -- locations of the components on the vertical axis
}
  \item{x}{
optional -- used to provide the range of the horizontal axis
}
  \item{ord}{
if TRUE orders the components by total entropy of each fitted model.
}
  \item{type}{
should the plot break down entropy by species (columns = default) or sites (rows).
}
  \item{labs}{
an (optional) vector of labels for species (or sites if type == "sites").
}
  \item{segs}{
use segments to plot end of bars as vertical lines
}
  \item{wid.seg}{
widths of bar ends
}
  \item{pchs}{
some nice plotting characters
}
  \item{\dots}{
other arguements passed to the plot function (e.g. cex).
}
}
\value{
a list of plotted values of the length of lst, each component of which is
of length equal to the number of species (sites).
}
\seealso{
\code{\link{mdm}}
}
\examples{
data(spider6)
fit0 <- mdm(y2p(spider6[,1:6])~1,data=spider6)
fit1 <- mdm(y2p(spider6[,1:6])~Water+Herbs,data=spider6)
fit2 <- mdm(y2p(spider6[,1:6])~Site,data=spider6,alpha=TRUE)
anova(fit0,fit1,fit2)
entropy.plot(list(fit0,fit2,fit1))
}


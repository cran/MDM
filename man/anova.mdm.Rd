\name{anova.mdm}
\title{Analysis of Deviance, Entropy and Diversity Tables}
\usage{
\method{anova}{mdm}(object, ..., topnote = TRUE, cols = c("df","dev","ent","div")[1:4])
}
\alias{anova.mdm}

\description{
Provides an analysis of deviance, entropy and diversity for a collection of diversity models (outputs from \code{mdm}).
}
\arguments{
\item{object, \dots}{objects of class \code{mdm}, usually, a result of a
    call to \code{\link{mdm}}.}

  \item{topnote}{
If TRUE then model descriptions appear above the anova table, if FALSE
they appear as the first column of the table
}

  \item{cols}{
The list of colums to print out. Defaults to all columns.
}
}
\details{
  Specifying a single object gives a sequential analysis of deviance
  table for that fit. That is, the reductions in the residual sum of
  deviances, plus the residual deviance
  The deviances are converted into entropies and diversities, and differences
  in deviances are converted into differnces in entropies and diversities.

  If more than one object is specified, the table has a row for the
  residual degrees of freedom and sum of deviances for each model.
  For all but the first model, the change in degrees of freedom and
  deviances. This only makes statistical sense if the models are nested.
  It is conventional to list the models from  smallest to largest,
  but this is up to the user.
}

\value{
The analysis of deviance, entropy and diversity for a collection of diversity models.
}
\references{
De'ath, G. (2011) \emph{The Multinomial Diversity Model: Linking Shannon Diversity
To Multiple Predictors}.

Ripley, B. D. (1996) \emph{Pattern Recognition and Neural Networks}. Cambridge.

Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S.}
Fourth edition. Springer.
}
\note{
\code{\link{mdm}} is a modifed version of \code{\link[nnet]{multinom}} in the
\code{\link{nnet}} package.
}
\seealso{
\code{\link[nnet]{multinom}}, \code{\link{nnet}}
}
\examples{
library(MDM)
data(spider6)
fit0 <- mdm(y2p(spider6[,1:6])~1,data=spider6)
fit1 <- mdm(y2p(spider6[,1:6])~Water,data=spider6)
fit2 <- mdm(y2p(spider6[,1:6])~Water+Herbs,data=spider6)
fit3 <- mdm(y2p(spider6[,1:6])~Site,data=spider6)
anova(fit0,fit1,fit2,fit3)
}

\keyword{mdm}


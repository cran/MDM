\name{mdm}
\alias{mdm}
\title{Fits the parameteric diversity model}
\description{
The parametric diversity model (mdm) is a new method for directly relating diversity to
environmental predictors. It is based on three components: (1) parametric diversity, a new
parametric model of diversity that can represent any configuration of species proportional
abundances, (2) the multinomial logit model (MLM) that can relate species proportional
abundances to complex predictors and (3) the link between parametric diversity and the
likelihood function of the MLM.

The \code{mdm} is fitted using the \code{multinom} package from \code{nnet}.
Parametric diversities and true diversities can also be calculated for data sets using
the functions \code{\link{eds}}, \code{\link{eds1}}, \code{\link{ed}}, \code{\link{ed1}}.
}
\usage{
mdm(formula, data, weights, subset, na.action,
    MaxNWts, maxit = 1000, contrasts = NULL, Hess = FALSE,
    censored = FALSE, model = TRUE, use.shortcut = TRUE, ...)
}
\arguments{
  \item{formula}{
a formula expression as for regression models, of the form response ~ predictors. The response
should be a matrix with K columns comprising proportions for each of K classes. A log-linear model
is fitted, with coefficients zero for the first class. An offset can be included:
it should be a numeric matrix with K columns. See the documentation of formula() for other details.
}
  \item{data}{
an optional data frame, list or environment (or object coercible by as.data.frame to a data frame)
containing the variables in the model. If not found in data, the variables are taken from
environment(formula), typically the environment from which \code{\link{mdm}} is called.
}
  \item{weights}{
optional case weights in fitting.
}
  \item{subset}{
expression saying which subset of the rows of the data should be used in the fit.
All observations are included by default.
}
  \item{na.action}{
a function to filter missing data.
}
  \item{MaxNWts}{
The maximum allowable number of weights. There is no limit in the code, but MaxNWts
is set to the exact number of required as specified in the formula.
Thus it should not need to be changed when fitting \code{\link{mdm}}.
}
  \item{maxit}{
maximum number of iterations. Default 1000.
}
  \item{contrasts}{
a list of contrasts to be used for some or all of the factors appearing as variables
in the model formula.
}
  \item{Hess}{
logical for whether the Hessian (the O/E information matrix) should be returned.
}
  \item{censored}{
If Y is a matrix with K > 2 columns, interpret the entries as one for possible classes,
zero for impossible classes, rather than as counts.
}
  \item{model}{
logical. If true, the model frame is saved as component model of the returned object.
}
  \item{use.shortcut}{
logical. If true, and the model is ~1 (a constant) or ~sites (a factor with one level for each site)
then the model is not fitted since the fitted values are known in each case. The first (alpha) model
has fitted values equal to the input data and the second (gamma) model fits the row means.
Fitting the alpha-model using nnet can be prohibitively expensive in computational time and
is unneccessary.
The returned models when use.shortcut == TRUE has the same components as when use.shortcut == FALSE,
and hence can be used in anova tables and plotting.
Using use.shortcut == TRUE can result in saving > 99\% of computational time for a collection of models.
}
  \item{\dots}{
  additional arguments for nnet.
}
}
\details{
\code{\link{mdm}} calls \code{\link[nnet]{multinom}} which calls \code{\link{nnet}}.
The variables on the rhs of the formula should be roughly scaled to [0,1]
or the fit will be slow or may not converge at all.
}
\value{
A nnet object with additional components:

\item{deviance}{the residual deviance, compared to the full saturated model
(that explains individual observations exactly). Also, minus twice log-likelihood}
\item{edf}{the (effective) number of degrees of freedom used by the model}
\item{AIC}{the AIC for this fit}
\item{Hessian}{if Hess is true}
\item{model}{if model is true}
\item{entropy}{the entropy of the fitted values}
\item{diversity}{the diversity of the fitted values}
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

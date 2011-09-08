anova.mdm <- function (object, ...)
{
    dots <- list(...)
    if (length(dots) == 0)
        stop("anova is not implemented for a single \"mdm\" object")
    mlist <- list(object, ...)
    nt <- length(mlist)
    dflis <- sapply(mlist, function(x) x$edf)
    s <- order(dflis)
    dflis <- nrow(object$residuals) * (ncol(object$residuals) - 1) - dflis
    mlist <- mlist[s]
    if (any(!sapply(mlist, inherits, "mdm")))
        stop("not all objects are of class \"mdm\"")
    ns <- sapply(mlist, function(x) length(x$residuals))
    if (any(ns != ns[1L]))
        stop("models were not all fitted to the same size of dataset")
    rsp <- unique(sapply(mlist, function(x) paste(formula(x)[2L])))
    mds <- sapply(mlist, function(x) paste(formula(x)[3L]))
	nr <- nrow(mlist[[1]]$residuals)
    dfs <- dflis[s]
    lls <- sapply(mlist, function(x) deviance(x))
    tss <- c("", paste(1L:(nt - 1), 2L:nt, sep = " vs "))
    df <- c(NA, -diff(dfs))
    x2 <- c(NA, -diff(lls))
    pr <- c(NA, 1 - pchisq(x2[-1L], df[-1L]))
	ent <- lls/2/nr
    dent <- c(NA, -diff(ent))
    div <- exp(ent)
    ddiv <- exp(dent)
    out <- data.frame(Model = mds, Resid.df = dfs, Deviance = lls,
         df = df, ddev = x2, ent = ent, dent = dent, div = div, ddiv = ddiv)
    names(out) <- c("Model", "Res-DF", "Res-Dev",
        "   DF", "  Dev","Ent", "Ent-Diff","Div", "Div-Ratio")
	rownames(out) <- as.character(sapply(sapply(sapply(mlist,"[[","call"),"[[",2),"[[",3))
    class(out) <- c("anova", "data.frame")
    attr(out, "heading") <- c("Deviances, Entropies and Diversities of Parametric Diversity Models\n",
        paste("Response:", rsp))
    out
}


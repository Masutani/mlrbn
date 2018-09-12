library(stringi)
library(BBmisc)

testdata = data.frame(
                        a = c("a", "b", "a", "b", "a", "b"),
                        b =  runif(6),
                        c = c("e", "f", "d", "g", "f", "e"),
                        d = c("a", "b", "a", "b", "a", "b"),
                        e = c("3", "2", "4", "2", "3", "4"),
                        f = factor(c("0", "1", "1", "0", "1", "0")),
                        g = factor(c(0, 1, 0,1, 1, 1)),
                        h = factor(c(0, 1, 0, -1, 1, 1)),
                        i = c(0, 1, 1, 0, 1, 1),
                        j = factor(c(2, 2, 4, 2, 6, 3)),
                        k = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE),
                        l = c("FALSE", "TRUE", "FALSE", "TRUE", "TRUE", "FALSE")
                        )

makeNumerizeWrapper = function(learner) {
    learner = checkLearner(learner)
    args = list()
    rm(list = names(args))

    trainfun = function(data, target, args) {
        ### Identify factor features
        cns = colnames(data)
        factors = setdiff(cns[sapply(data, is.factor)], target)
        ### Identify binary features
        fdata <- data[, factors, drop = FALSE]
        bins = factors[sapply(fdata, canLogical)]
        nbins = setdiff(factors, bins)
        ### Convert binary features to logical and others to numerical
        ndata <- factor2Numeric(data, bins, nbins)
        ### Recombine the data
        data = data[, setdiff(cns, factors), drop = FALSE]
        data = cbind(data, ndata)
        return(list(data = data, control = list(bins = bins, nbins = nbins)))
    }

    predictfun = function(data, target, args, control) {
        ### Identify factor features
        cns = colnames(data)
        factors = c(control$bins, control$nbins)
        ### Extract numerical features from the data set and call scale
        ndata <- factor2Numeric(data, control$bins, control$nbins)
        ### Recombine the data
        data = data[, setdiff(cns, factors), drop = FALSE]
        data = cbind(data, ndata)
        return(data)
    }

    lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = args)
    lrn$id = stri_replace(lrn$id, replacement = ".numerized", regex = "\\.preproc$")
    addClasses(lrn, "NumerizeWrapper")
}

getLearnerProperties.NumerizeWrapper = function(learner) {
    union(getLearnerProperties(learner$next.learner), "factors")
}

factor2Numeric = function(data, bins, nbins) {
    ### Convert binary features to logical and others to numerical
    bdata = data[, bins, drop = FALSE]
    nbdata = data[, nbins, drop = FALSE]
    numbdata <- as.data.frame(lapply(bdata, toLogical))
    numnbdata <- as.data.frame(lapply(nbdata, as.numeric))

    if (length(bins) == 0) {
        return(numnbdata)
    } else if (length(nbins) == 0)  {
        return(numbdata)
    }
    data = cbind(numbdata, numnbdata)
    return(data)
}


canLogical = function(x, binaryAssignment = c(0,1)) {
    if (is.factor(x)) {
        if (length(levels(x)) == 2) {
            if (all(!is.na(as.logical(levels(x))))) {
                return(TRUE)
            }
            else if (all(as.numeric(levels(x)) %in% binaryAssignment)) {

                return(TRUE)
            }
        }
    }
    return(FALSE)
}

toLogical = function(x, binaryAssignment = c(0, 1)) {
    if (is.factor(x)) {
        if (length(levels(x)) == 2) {
            if (all(!is.na(as.logical(levels(x))))) {
                return(as.numeric(as.logical(x)))
            }
            else if (all(as.numeric(levels(x)) %in% binaryAssignment)) {
                return(as.numeric(as.logical(x == binaryAssignment[2])))
            }
        }
    }
    return(x)
}




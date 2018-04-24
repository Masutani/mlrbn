#' bnlearn wrapper for mlr multi label learner

#' make leaner
#' 
#' @export
makeRLearner.multilabel.bnlearn = function() {
    makeRLearnerMultilabel(
    cl = "multilabel.bnlearn",
    package = "bnlearn",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "structuremethod", default = "hc", values = c("hc","tabu")),
      makeDiscreteLearnerParam(id = "fitmethod", default = "mle", values = c("mle", "bayes")),
      makeNumericLearnerParam(id = "thresh", default = 1.0, lower = 0)
    ),
    properties = c("numerics", "factors", "prob"),
    name = "bayesian networks classifier by bnlearn",
    short.name = "bnlearn",
    note = ""
  )
}

#' train leaner
#' @param .learner learner to be trained
#' @param .task multi label task
#' @param .subset sampling index
#' @param .weights weight for feature vector
#' @param structuremethod param for bnlearn
#' @param fitmethod param for bnlearn
#'
#' @export
trainLearner.multilabel.bnlearn = function(.learner, .task, .subset, .weights = NULL, structuremethod = "hc", fitmethod = "mle", ...) {
    d = getTaskData(.task, .subset, target.extra = TRUE)
    totalcols = ncol(d$target) + ncol(d$data)
    totalcolnames = c(colnames(d$target), colnames(d$data))
    bl = data.frame(from = rep(colnames(d$target), times = 1, each = totalcols), to = rep(totalcolnames, times = ncol(d$target), each = 1))
    nulab = apply(d$target, 2, as.numeric)
    data = cbind(nulab, d$data)
    if (structuremethod == "hc") {
        structure = hc(data, blacklist = bl)
    } else if(structuremethod == "tabu") {
        structure = tabu(data, blacklist = bl)
    }
    fitted = bn.fit(structure, data, method = fitmethod)
}
#' predict leaner
#' @param .learner learner to be used
#' @param .model trained model
#' @param .newdata data for prediction
#' 
#' @export
predictLearner.multilabel.bnlearn = function(.learner, .model, .newdata,  ...) {
    target = .model$task.desc$target
    dummylabel = matrix(, nrow = nrow(.newdata), ncol = length(target))
    colnames(dummylabel) = target
    data = cbind(dummylabel, .newdata)
    preds <- foreach(i = 1:length(target), .combine = 'cbind') %do% {
        pred <- predict(.model$learner.model, node = target[i], .newdata, ...)
    }
    colnames(preds) = target
    param <- .model$learner$par.vals

    if (.learner$predict.type == "response") {
        pred2 = as.matrix(preds > param$thresh)
        return(pred2)
    } else if (.learner$predict.type == "prob") {
        return(preds)
    }
}

#' bayes server wrapper for mlr as multi label classifier

#' make leaner
#' 
#' @export
makeRLearner.multilabel.bayesserver = function() {
    cat(sprintf("make learner : bayesserver \n"))

    makeRLearnerMultilabel(
    cl = "multilabel.bayesserver",
    package = "bnlearn",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "structuremethod", default = "pc", values = c("pc", "search", "hierachical", "chow-liu", "tan")),
      makeNumericLearnerParam(id = "thresh", default = 1.0, lower = 0)
    ),
    properties = c("numerics", "factors", "prob"),
    name = "bayesian networks classifier by BayesServer",
    short.name = "bayesserver",
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
trainLearner.multilabel.bayesserver = function(.learner, .task, .subset, .weights = NULL, structuremethod = "pc", ...) {
    cat(sprintf("train learner : bayesserver \n"))
    d = getTaskData(.task, .subset, target.extra = TRUE)
    totalcols = ncol(d$target) + ncol(d$data)
    totalcolnames = c(colnames(d$target), colnames(d$data))
   # bl = data.frame(from = rep(colnames(d$target), times = 1, each = totalcols), to = rep(totalcolnames, times = ncol(d$target), each = 1))
    data = cbind(d$target, d$data)

    fitted = trainNetwork(data, structuremethod)

}

#' predict leaner
#' @param .learner learner to be used
#' @param .model trained model
#' @param .newdata data for prediction
#' 
#' @export
predictLearner.multilabel.bayesserver = function(.learner, .model, .newdata, ...) {
    target = .model$task.desc$target

    probs <- inferNetwork(.model$learner.model, .newdata)
    probs <- probs[, target]

    param <- .model$learner$par.vals

    if (.learner$predict.type == "response") {
        pred2 = as.matrix(probs > param$thresh)
        return(pred2)
    } else if (.learner$predict.type == "prob") {
        return(as.matrix(probs))
    }
}
#' @title Additional performance measures for mlrbn.
#' @name measures.mlrbn
#' @import mlr

#' measure positive predictive value for multilabel
#' @export multilabel.ppv1
#' @rdname measures
#' @format none
multilabel.ppv1 = makeMeasure(
  id = "multilabel.ppv1", name = "positive predictive value in multiple truth label",
  properties = c("classif.multi","multilabel", "req.pred", "req.truth"),
  minimize = FALSE, best = 1, worst = 0,
  fun = multilabel.ppv1.fun
)

#' internal function for measure ppv1
#' 
multilabel.ppv1.fun = function(task, model, pred, feats, extra.args) {
    truth = getPredictionTruth(pred)
    response = getPredictionResponse(pred)
    numerator = sum(rowSums(truth & response) > 0)
    denominator = sum(rowSums(response) > 0)
    ppv1 = mean(numerator / denominator, na.rm = TRUE)
    #    matchn = apply(match, MARGIN = c(1, 2), as.numeric)
    #    ppv1 <- sum(rowSums(matchn) > 0) / nrow(matchn)
    return(ppv1)
}
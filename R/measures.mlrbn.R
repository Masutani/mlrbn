#' Additional performance measures for mlrbn.
#' measures.mlrbn

#' internal function for measure ppv1
#' 
ppv1fun = function(task, model, pred, feats, extra.args) {
    truth = getPredictionTruth(pred)
    response = getPredictionResponse(pred)
    numerator = sum(rowSums(truth & response) > 0)
    denominator = sum(rowSums(response) > 0)
    ppv1 = mean(numerator / denominator, na.rm = TRUE)
    #    matchn = apply(match, MARGIN = c(1, 2), as.numeric)
    #    ppv1 <- sum(rowSums(matchn) > 0) / nrow(matchn)
    return(ppv1)
}

#' measure positive predictive value for multilabel
#' @rdname measures
#' @format none
#' @export
multilabel.ppv1 = makeMeasure(
  id = "multilabel.ppv1", name = "positive predictive value in multiple truth label",
  properties = c("classif.multi","multilabel", "req.pred", "req.truth"),
  minimize = FALSE, best = 1, worst = 0,
  fun = ppv1fun
)


#' Variables' Importance
#'
#' Showing the importance of variables in model.
#'
#' @param model_tuned result given by function \code{\link{tune}}
#' @return a data.table shown the importance of features
#' @import mlr
#' @import xgboost
#' @export
var_imp <- function(model_tuned) {
  features <- model_tuned$model$features
  var_importance <- xgb.importance(feature_names = features,
                                   model = model_tuned$model$learner.model)
  var_importance
}

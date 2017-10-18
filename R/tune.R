#' XGBoost Parameter's Tuning
#'
#' Provide a standard process for parameter tuning for \code{xgboost} model.
#'
#' @param data cleaned data set with all numeric variable
#' @param target name of target variable in \code{data}
#' @param cv.folds n-fold cross validation
#' @param cv.rep times to repeat n-fold cross validation
#' @param stratified whether to performance stratified sampling for cross validation
#' @param postive name of positive class in target
#'
#' @return A list containing the parameter settings and a model wrapped in \code{mlr} model object.
#' @slot model an \code{mlr} model object
#' @slot parameters parameter setting tuned
#'
#' @import mlr
#' @import parallel
#' @export
tune <- function(data, target, cv.folds = 10,
                 cv.rep = 10, stratified = TRUE,
                 positive = NA_character_,
                 alpha_values = c(0, 2 ^ (-2:3)),
                 lambda_values = c(0, 2 ^ (-2:3)),
                 gamma_values = c(0:10 / 10, 2^(1:3))) {
  task <- makeClassifTask(data = data,
                          target = target,
                          positive = positive)
  lnr  <- makeLearner('classif.xgboost',
                      predict.type = 'prob')
  resample_desc <- makeResampleDesc(method = 'RepCV',
                                    folds = cv.folds,
                                    reps = cv.rep,
                                    stratify = stratified)

  n_obs <- nrow(data)
  # number of observations in minority class
  minor_num <- min(table(data[, target]))

  # to set subsample parameter for xgboost
  subsample <- max(min(1, 1000 / minor_num), 0.5)
  colsample_bytree <- 1
  par_vals <- list(eta = 0.1,
                   subsample = subsample,
                   colsample_bytree = colsample_bytree,
                   nthread = detectCores(logical = FALSE))

  lnr <- setHyperPars(lnr, par.vals = par_vals)
  tuning_ctrl <- makeTuneControlGrid()

  ### 0. Set nrounds
  nrounds_setting <- makeParamSet(
    makeDiscreteParam('nrounds', values = 1:5 * 50)
  )
  nrounds_result <- tuneParams(learner = lnr,
                               task = task,
                               resampling = resample_desc,
                               control = tuning_ctrl,
                               measures = auc,
                               par.set = nrounds_setting)
  par_vals <- c(par_vals, nrounds_result$x)
  lnr <- setHyperPars(lnr, par.vals = par_vals)

  ### 1. Set max_depth
  depth_setting <- makeParamSet(
    makeDiscreteParam('max_depth', values = 1:10)
  )
  depth_result <- tuneParams(learner = lnr,
                             task = task,
                             resampling = resample_desc,
                             control = tuning_ctrl,
                             measures = auc,
                             par.set = depth_setting)
  par_vals <- c(par_vals, depth_result$x)
  lnr <- setHyperPars(lnr, par.vals = par_vals)

  ### 2. Set regulation coefficients
  regu_setting <- makeParamSet(
    makeDiscreteParam('alpha', values = alpha_values),
    makeDiscreteParam('lambda', values = lambda_values)
  )
  regu_result <- tuneParams(learner = lnr,
                            task = task,
                            resampling = resample_desc,
                            control = tuning_ctrl,
                            measures = auc,
                            par.set = regu_setting)
  par_vals <- c(par_vals, regu_result$x)
  lnr <- setHyperPars(lnr, par.vals = par_vals)

  ### 2. Set gamma
  gamma_setting <- makeParamSet(
    makeDiscreteParam('gamma', values = gamma_values)
  )
  gamma_result <- tuneParams(learner = lnr,
                            task = task,
                            resampling = resample_desc,
                            control = tuning_ctrl,
                            measures = auc,
                            par.set = gamma_setting)
  par_vals <- c(par_vals, gamma_result$x)
  lnr <- setHyperPars(lnr, par.vals = par_vals)

  xgb_mod <- train(learner = lnr,
                   task = task)

  list(
    model = xgb_mod,
    parameters = par_vals
  )
}

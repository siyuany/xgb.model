% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tune.R
\name{tune}
\alias{tune}
\title{XGBoost Parameter's Tuning}
\usage{
tune(data, target, cv.folds = 10, cv.rep = 10, stratified = TRUE,
  positive = NA_character_, alpha_values = c(0, 2^(-2:3)),
  lambda_values = c(0, 2^(-2:3)), gamma_values = c(0:10/10, 2^(1:3)))
}
\arguments{
\item{data}{cleaned data set with all numeric variable}

\item{target}{name of target variable in \code{data}}

\item{cv.folds}{n-fold cross validation}

\item{cv.rep}{times to repeat n-fold cross validation}

\item{stratified}{whether to performance stratified sampling for cross validation}

\item{postive}{name of positive class in target}
}
\value{
A list containing the parameter settings and a model wrapped in \code{mlr} model object.
}
\description{
Provide a standard process for parameter tuning for \code{xgboost} model.
}
\section{Slots}{

\describe{
\item{\code{model}}{an \code{mlr} model object}

\item{\code{parameters}}{parameter setting tuned}
}}


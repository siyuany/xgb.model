#' Feature Selection Based on Pearson and Spearman Correlation
#'
#' @param var_imp Variables' importance given by \code{\link{var_imp}}
#' @param data the data set
#' @param pearson_threshold Threshold for variable filtering on Pearson correlation, will be neglected if \code{threshold} is set
#' @param spearman_threshold Threshold for variable filtering on Spearman correlation, will be neglected if \code{threshold} is set
#' @param threshold threshold for variable filtering on both Pearson and Spearman correlation, will overwrite \code{pearson_threshold} and \code{spearman_threshold}
#' @return features selected
#' @export

cor_filter <- function(var_imp, data, threshold = NA_real_,
                       pearson_threshold = 0.7,
                       spearman_threshold = 0.7) {
  features <- var_imp$Feature
  n_feat <- length(features)

  if (!is.na(threshold)) {
    pearson_threshold <- threshold
    spearman_threshold <- threshold
  }

  for (i in n_feat:2) {
    feat <- data[[features[i]]]

    for (j in 1:(i - 1)) {
      higher_feat <- data[[features[j]]]

      s_cor <- cor(feat, higher_feat,
                   use = 'complete.obs',
                   method = 'spearman')
      p_cor <- cor(feat, higher_feat,
                   use = 'complete.obs',
                   method = 'spearman')

      if (abs(s_cor) > spearman_threshold | abs(p_cor) > pearson_threshold) {
        features[i] <- NA_character_
        message(paste0("Drop: ", features[i], "\n"))
        break
      }
    }
  }

  features[!is.na(features)]
}

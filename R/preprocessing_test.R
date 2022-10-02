#' preprocessing_test
#'
#' This function is used to test the performance of different pre-processing test
#'
#' @param spec Input spectral data df/matrix
#' @param reference Chemical reference data
#' @param target target chemical property
#' @param seed set.seed for repeat checking
#'
#' @return Dataframe containing performance indicators
#' @export
#'
#' @examples
preprocessing_test <- function(spec, reference, target, seed){

  df <- data.frame('BD' = reference)
  df$spec <- spec

  pca <- stats::prcomp(df$spec)
  indicator <- summary(pca)[[6]][,1:10]
  pcs <- which.min(abs(indicator[3,] - 0.9))
  scores <- as.data.frame(pca$x[,1:pcs])
  keys <- outlier(scores, pcs, cutoff=0.975)

  df <- df[-keys,]

  set.seed(seed)
  temp <- data_spliting(df, method = 'Venetian', p = 3, target=target)

  train_x <- temp$train[,-which(colnames(temp$train) %in% c(target))]
  train_y <- temp$train[,which(colnames(temp$train) %in% c(target))]
  test_x <- temp$test[,-which(colnames(temp$test) %in% c(target))]
  test_y <- temp$test[,which(colnames(temp$test) %in% c(target))]

  train_x <- as.data.frame(train_x)
  test_x <- as.data.frame(test_x)

  model <- pls::plsr(train_y ~ ., data = train_x, ncomp=30, scale=FALSE, validation='CV', center = TRUE)

  Result <- data.frame('R2'=numeric(0),'RMSEP'=numeric(0),'RPD'=numeric(0),'RPIQ'=numeric(0))

  for (i in 1:30){
    test_pred <- c(stats::predict(model, newdata=test_x, ncomp=i, Classification='response'))
    temp_performance <- as.data.frame(performance(test_pred, test_y, cat = i))
    Result <- rbind(Result, temp_performance)
  }

  return(Result)
}

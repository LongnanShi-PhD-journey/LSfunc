#' Mean centering
#'
#' @param df Data Frame with col names and row names
#'
#' @return Data Frame with col names an row names
#' @export
#'
#' @examples
MC <- function(df){

  # Description: Pre-processing method for spectral data：Centering all columns by mean
  #
  # Input: Data Frame with col names and row names
  #
  # Output: Data Frame with col names an row names

  temp <- as.matrix(df)
  result <- matrix(NA, ncol = ncol(df), nrow = nrow(df))

  for (i in 1:ncol(df)){
    avg <- mean(temp[,i])
    result[,i] <- (temp[,i] - avg)
  }

  rownames(result) <- rownames(df)
  colnames(result) <- colnames(df)
  return(result)
}

# Standard Normal Variate-------------------------------------------------------
#' Title SNV
#'
#' @param df Data Frame with col names and row names
#'
#' @return Data Frame with col names an row names
#' @export
#'
#' @examples
SNV <- function(df){

  # Description: Pre-processing method for spectral data: SNV which is also known
  #              as z-transformation or as centring and scaling (operating per
  #              spectrum or row-wise) normalizes each spectrum to zero mean and
  #              unit variance by subtracting the mean of the spectrum and
  #              dividing the difference by its standard deviation
  #
  # Input: Data Frame with col names and row names
  #
  # Output: Data Frame with col names an row names

  temp <- as.matrix(df) # convert dataframe to matrix
  result <- matrix(NA, ncol = ncol(df), nrow = nrow(df)) # create a new empty matrix, ncol(df) * nrow(df)

  for (i in 1:nrow(df)){
    avg <- mean(temp[i,])
    std <- stats::sd(temp[i,])
    result[i,] <- (temp[i,] - avg)/std
  }

  rownames(result) <- rownames(df)
  colnames(result) <- colnames(df)
  return(result)
}


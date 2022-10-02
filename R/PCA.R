# Principle Component Analysis(PCA)---------------------------------------------
#' Principle Component Analysis(PCA)
#'
#' @param df Dataframe
#' @param PCs How many PCs showed in summary and plots
#' @param ifplot if plot explained variance and cumulative explained variance when run function
#'
#' @return prcomp objects
#' @export
#'
#' @examples
pca <- function(df, PCs=10, ifplot = TRUE){

  # Description: Principal component analysis (PCA) is a statistical technique
  #              used to examine the interrelations among a set of variables and
  #              to identify their underlying structure.
  #
  # Input: df:Dataframe
  #        PCs: How many PCs showed in summary and plots
  #        ifplot: if plot explained variance and cumulative explained variance
  #               when run function
  #
  # Output: prcomp objects


  temp <- stats::prcomp(df)
  if (ifplot==TRUE){
    graphics::par(mfrow=c(1,2))
    plot(summary(temp)[[6]][,1:PCs][2,],
         type = 'o',
         xlab = 'PCs',
         ylab = 'Explained Variance',
         main = 'Explained Variance')

    plot(summary(temp)[[6]][,1:PCs][3,],
         type = 'o',
         xlab = 'PCs',
         ylab = 'Cumulative Explained Variance',
         main = 'Cumulative Explained Variance')
    graphics::par(mfrow=c(1,1))
  }
  print(summary(temp)[[6]][,1:PCs])
  return(temp)
}

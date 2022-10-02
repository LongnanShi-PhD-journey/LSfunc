# Biplot and Score-Loading plot-------------------------------------------------------

#' Biplot and Score-Loading plot
#'
#' @param df The dataframe of chemical data and scores
#' @param a PC, in string. e.g. 'PC1'
#' @param b PC, in string. e.g. 'PC1'
#' @param cat Category column used for plot
#' @param indicator The dataframe including explained variance, obtained by following code indicator<- as.data.frame(summary(pca_objects)[[6]][,1:10])
#'
#' @return Figures with biplots and Loading plot
#' @export
#'
#' @examples
biplot_PCA <- function(df, a,b, cat, indicator){

  # Descriptions: This function is used to plot the biplot.
  #               To identify the variability of spectral data with specific soil
  #               properties
  #
  # Input: df: The dataframe of chemical data and scores
  #        a: PC, in string. e.g. 'PC1'
  #        b: PC, in string. e.g. 'PC1'
  #        cat: Category column used for plot
  #        indicator: The dataframe including explained variance, obtained by following code
  #                   indicator<- as.data.frame(summary(MIR_pca)[[6]][,1:10])
  #
  # Output: Figures with biplots and Loading plot


  # plot scatter points, vertical(x=0) and horizontal(y=0) line
  biplot <-
    ggplot2::ggplot(df, ggplot2::aes_string(x=a, y=b, color = cat))+
    ggplot2::geom_point(size = 2.0, shape = 16)+
    ggplot2::geom_hline(yintercept=0, linetype="dashed", color = "red")+
    ggplot2::geom_vline(xintercept=0, linetype="dashed", color = "red")

  # add xlab and ylab with explained variance
  biplot <-
    biplot +
    ggplot2::xlab(paste(a, ':', scales::percent(indicator['Proportion of Variance',a]))) +
    ggplot2::ylab(paste(b, ':', scales::percent(indicator['Proportion of Variance',b])))

  # Set text size
  biplot <- biplot +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 16),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.y = ggplot2::element_text(size = 16),
                   plot.title = ggplot2::element_text(size = 20, face = "bold", color = "darkgreen"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))
  return(biplot)
}

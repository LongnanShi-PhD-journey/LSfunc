#' Biplot and Score-Loading plot
#'
#' @param df The dataframe of PCs loading, containing information about wavenumber
#' @param PC_num selected PC, in string, e.g. 'PC1'
#' @param indicator The dataframe including explained variance, obtained by following code indicator<- as.data.frame(summary(MIR_pca)[[6]][,1:10])
#'
#' @return Loading plot
#' @export
#'
#' @examples
loading_plot <- function(df, PC_num, indicator){

  # Descriptions: This function is used to plot loading plot of select PC, and identify peaks and valleys
  #
  # Input: df: The dataframe of PCs loading, containing information about wavenumber
  #        PC_num: selected PC, in string, e.g. 'PC1'
  #        indicator: The dataframe including explained variance, obtained by following code
  #                   indicator<- as.data.frame(summary(MIR_pca)[[6]][,1:10])
  #
  # Output: Loading plot


  temp <- df[colnames(df) == PC_num]
  peaks <- as.numeric(rownames(df)[photobiology::find_peaks(unlist(c(temp[1])),
                                                            span=99, ignore_threshold = 0.6)]) # find peaks
  valleys <- as.numeric(rownames(df)[photobiology::find_peaks(unlist(c(-temp[1])),
                                                              span=99, ignore_threshold = 0.9)]) # find valleys

  peaks_y = temp[rownames(df) %in% peaks,] # return wavenumber of peaks
  valleys_y = c(temp[rownames(df) %in% valleys,]) # return wavenumber of valleys

  annotation_peaks <- data.frame(x = peaks, y = peaks_y+0.01,label = paste(round(peaks))) # set peaks dataframe for plot
  annotation_valleys <- data.frame(x = valleys, y = valleys_y-0.01,label = paste(round(valleys))) # set valleys dataframe for plot

  # line plot with horizontal line(y=0)
  loading <-
    ggplot2::ggplot(data=df, ggplot2::aes_string(x=as.numeric(rownames(df)), y=PC_num))+
    ggplot2::geom_line()+
    ggplot2::labs(x = "Wavenumber /cm")+
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank())+
    ggplot2::geom_hline(yintercept=0, linetype="dashed", color = "red")+
    ggplot2::scale_x_reverse(limits = c(4000, 600))

  # add peaks on plot
  loading<-
    loading +
    ggplot2::geom_text(data=annotation_peaks, ggplot2::aes(x=x, y=y, label=label),
                       color="red", size=4)
  # add valleys on plot
  loading<-
    loading +
    ggplot2::geom_text(data=annotation_valleys, ggplot2::aes(x=x, y=y, label=label),
                       color="blue", size=4)

  # add ylab
  loading <-
    loading +
    ggplot2::ylab(paste(PC_num, ':', scales::percent(indicator['Proportion of Variance',paste(PC_num)])))

  # set text size
  loading <-
    loading +
    ggplot2::theme(legend.text = ggplot2::element_text(size=12),
                   legend.title = ggplot2::element_text(15),
                   axis.text.x = ggplot2::element_text(size = 10),
                   axis.text.y = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 12))

  return(loading)
}

library(ggplot2)
# ----- Define a function for plotting a matrix ----- #
myImagePlot <- function(x, ...){
  min <- min(x)
  max <- 1
  yLabels <- rownames(x)
  xLabels <- colnames(x)
  title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
      min <- Lst$zlim[1]
      max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
      yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
      xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
      title <- Lst$title
    }
  }
  # check for null values
  if( is.null(xLabels) ){
    xLabels <- c(1:ncol(x))
  }
  if( is.null(yLabels) ){
    yLabels <- c(1:nrow(x))
  }
  # 建立heatmap資料表，並放入參數
  xLabels <- xLabels
  yLabels <- yLabels
  x <- as.data.frame(x)
  colnames(x) <- xLabels
  x$topic <- factor(yLabels, levels = yLabels)
  x <- reshape2::melt(data = x)
  
  # Heatmap 
  g <- ggplot(x, aes(variable, topic, fill = value)) +
    geom_raster() +
    scale_fill_gradient(low = "#FFFFFF", high = "#000000")+
    scale_y_discrete(limits = rev(levels(x$topic))) +
    labs(x = "Year", y = NULL, title = title) +
    theme(text = element_text(size = 22),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black", size = .2),
          panel.background = element_rect(fill = NA),
          legend.key.height = unit(0.16, "npc"),
          panel.spacing = unit(1, "npc"),
          plot.title = element_text(hjust = .5)
    )
  
  return(g)

}
# ----- END plot function ----- #
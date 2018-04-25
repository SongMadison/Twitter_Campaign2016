
balloon.plot <- function(A, ylabel = NULL, xlabel = NULL , main = NULL){
  nr <- nrow(A)
  A <- A[nr:1,];  
  dat1 <- flattenMatrix(A)
  dat1$row <- as.factor(dat1$row); dat1$col <- as.factor(dat1$col)
  if (length(which(A<0))>0){
    dat1$sign <- as.factor(1*(dat1$value>0))
    dat1$sign <-factor(dat1$sign, levels =c(1,0), labels = c(">0","<0"))
    dat1$value <- abs(dat1$value)
  } 
  if (!is.null(ylabel)) {dat1$row <- factor(dat1$row, levels = 1:nrow(A), ylabel[nr:1])
  } else {dat1$row <- factor(dat1$row, levels = 1:nrow(A), as.character(nr:1) )}
  if (!is.null(xlabel)) { dat1$col <- factor(dat1$col, levels = 1:ncol(A), xlabel)}
  else{ dat1$col <- factor(dat1$col)}
  #dat1$ <-  dat1$value > quantile(dat1$value, 0.9)
  p <- ggplot(dat1,  aes(x = col, y= row ))
  
  if (length(which(A<0))>0){
    p1 <- p + geom_point(aes(size = value, colour = sign), shape = 16) +xlab('')+ylab('')
  }else{
    p1 <- p + geom_point(aes(size = value), color = "blue", shape = 16) +xlab('')+ylab('')
  }
  if (! is.null(xlabel)) {
    p1 <- p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  p1 + ggtitle(main) + scale_size(range = c(0.5,3))
}



# Multiple plot function
# copy and modified from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

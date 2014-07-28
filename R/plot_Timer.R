plot.Timer <- function(data, colors = 
  c(
    start = "white", spawn = "gray", train = "red", 
    join = "green", merge = "blue", parallelReduce = "orange",
    structure = "blue"
  )
){
    nthreads <- length(data)
    for(i in 1:nthreads) data[[i]] <- data[[i]] / 1e6
  
    xlim <- range(unlist(data))
    print(xlim)
    
    ylim <- c(0.5, length(data) + .5)
    
    par( mar = rep(.5,4) )
    plot( 0, type = "n", xlim = xlim, ylim = ylim, axes = FALSE, ann = FALSE )
    
    for( i in 1:length(data) ){
        x <- c( "start" = 0, data[[i]] )
        n <- length(x)
        names <- tail(names(x),-1)
        rect( xleft = head(x,-1), xright = tail(x,-1), 
          ybottom = rep(i-.5,n-1), 
          ytop = rep(i+0.5,n-1), 
          col = colors[names], 
          border = NA
          )   
    }
    
}


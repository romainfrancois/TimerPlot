color_choices <- c(
    start = "white", spawn = "gray", train = "royalblue", work = "royalblue", 
    join = "red", merge = "purple3", parallelReduce = "white",
    structure = "orange"
  )

draw_worker <- function(x, bottom, top, colors){
    n <- length(x)
    if( !n ) return() 
    names <- tail(names(x),-1)
    rect( 
        xleft = head(x,-1), 
        xright = tail(x,-1), 
        ybottom = rep(bottom,n-1), 
        ytop = rep(top,n-1), 
        col = colors[names], 
        # border = NA,
        border = ifelse(names=="start", NA, "black"), 
        lwd = .5  
    )   
      
}
  
  
plot.SingleTimer <- function(data, colors = color_choices , 
  xmax = max(unlist(data))
){
    nthreads <- length(data)
    for(i in 1:nthreads) data[[i]] <- data[[i]] / 1e6
    
    xlim <- c(0, xmax)
    ylim <- c(0,1)
    plot( 0, type = "n", xlim = xlim, ylim = ylim, 
        axes = FALSE, ann = FALSE, yaxs = "i", xaxs = "i" )
    draw_worker( data[[1]], bottom = 0, top = 1, colors )
    axis(1)
    box()
    
}

plot.FixedSizeTimers <- function(data, colors = color_choices , 
  xmax = max(unlist(data))
){
    nthreads <- length(data)
    for(i in 1:nthreads) data[[i]] <- data[[i]] / 1e6
    n_total <- attr( data, "n")
    xlim <- c(0, xmax)
    ylim <- c(0,1)
    plot( 0, type = "n", xlim = xlim, ylim = ylim, 
        axes = FALSE, ann = FALSE, yaxs = "i", xaxs = "i" )
    
    draw_worker( data[[1]], bottom = 0, top = 1, colors )
    
    bottom <- 0
    for( i in 2:length(data)){
      d <- data[[i]]
      top <- bottom + attr(d, "n") / n_total
      draw_worker( data[[i]], bottom = bottom, top = top, colors )
      bottom <- top
    }
    
    axis(1)
    box()
    
}


plot.TimersList <- function(data, colors = color_choices , 
  xmax = max(unlist(data))
){
    nthreads <- length(data)
    for(i in 1:nthreads) data[[i]] <- data[[i]] / 1e6
  
    n_total <- attr(data, "n")
    
    xlim <- c(0, xmax)
    ylim <- c(0, 1)
    plot( 0, type = "n", xlim = xlim, ylim = ylim, 
        axes = FALSE, ann = FALSE, yaxs = "i", xaxs = "i" )
    axis(1)
        
    draw_thread <- function(i, bottom = 0, depth = 0, n_total){
        d <- data[[i]]
        
        childs <- attr(d, "childs")
        for( child in childs){
            data_child <- data[[child]]
            top <- draw_thread(child, bottom, depth = depth + 1, n_total = n_total)
            bottom <- top
        }
        top <- bottom + attr(d,"n") / n_total
        
        rect( xleft = min(d), xright = max(d), ybottom = bottom, ytop = top, lwd = .5 )
        
        draw_worker(d, bottom, top, colors ) 
        
        top
    }
    draw_thread(2, n_total = n_total)
    draw_worker(data[[1]], 0, 1, colors ) 
        
    box()
    
}

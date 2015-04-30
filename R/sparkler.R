#' Convert ggplot object to LaTex
#'
#' This function converts a ggplot object to latex code. Requires \usepackage{sparklines} in your latex header.
#' @param gg_obj Ggplot object.
#' @param width Width, in em, for your sparkline.
#' @keywords sparkline
#' @export
#' @examples
#' sparkTex(qplot(rnorm(1e3), geom="histogram", binwidth=1))

sparkTex <- function(gg_obj, width=4) {
  if(!(prod(class(gg_obj)==c("gg","ggplot")))) {
    stop("gg_obj not a ggplot object")
  }
  geoms <- lapply(gg_obj$layers, function(x) x$geom$objname)
  build_obj <- ggplot_build(gg_obj)
  data.layers <- build_obj$data

  data.all <- list()
  for(i in 1:length(data.layers)) {
    data.all[[i]] <- list(data=data.layers[[i]],
                          geom=geoms[[i]])
  }

  body <- lapply(data.all, function(d) {
    switch(d$geom,
            density = {
              type <- "\\spark"
              y <- d$data$y
              y <- (y-min(y))/(max(y)-min(y))
              x <- d$data$x
              x <- (x-min(x))/(max(x)-min(x))
              idx <- round(seq(1,length(y),length=20))
              y <- y[idx]
              x <- x[idx]
              paste("\t",type," ", paste(x[order(x)],y[order(x)],collapse="  "), " /")
            },
            histogram = {
              type <- "\\sparkspike"
              y <- d$data$density
              y <- y/max(y)
              x <- seq(0,1,length=length(y)) 
              paste0("\t",type," ",x," ",y,collapse="\n")
            }, 
            line = {  
              type <- "\\spark"
              y <- d$data$y
              y <- (y-min(y))/(max(y)-min(y))
              x <- d$data$x
              x <- (x-min(x))/(max(x)-min(x))
              paste("\t",type," ", paste(x[order(x)],y[order(x)],collapse="  "), " /")
            },
            point = {
              type <- "\\sparkdot"
              color <- rep("black", times=length(d$data$x))
              y <- d$data$y
              y <- (y-min(y))/(max(y)-min(y))
              x <- d$data$x
              x <- (x-min(x))/(max(x)-min(x))
              paste("\t",type,x,y,color,collapse="\n")

            },
            stop("Only geom_density(), geom_histogram(), geom_line(), and geom_point() are currently supported"))    
  })
  body <- paste0(do.call(c,body),collapse="\n")

  paste0("\\begin{sparkline}{",width,"}\n",
         body,
         "\n\\end{sparkline}\n")

}

library(igraph)
data(expd.edges.1)
data(expd.nodes.1)

#' MyCircle to render nodes
#'
#' This function supports conversion of colors from the HSV scale to Hex scale. This function is a data.frame specific helper (wrapper) function that uses the built-in hsv function under the hood.
#' @param takes a row number (of the data frame that contains Hue, Saturation, and Value in columns 6, 7, and 8 respectively) as input.
#' @keywords color, hex, hsv
#' @export
#' @examples
#' mycircle()
mycircle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
        vertex.color <- vertex.color[v]
    }
  vertex.size  <- 1/200 * params("vertex", "size")
     if (length(vertex.size) != 1 && !is.null(v)) {
     vertex.size <- vertex.size[v]
    }
  vertex.frame.color <- params("vertex", "frame.color")
  if (length(vertex.frame.color) != 1 && !is.null(v)) {
    vertex.frame.color <- vertex.frame.color[v]
    }
  vertex.frame.width <- params("vertex", "frame.width")
  if (length(vertex.frame.width) != 1 && !is.null(v)) {
      vertex.frame.width <- vertex.frame.width[v]
      }
    mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
                       vertex.size, vertex.frame.width,
                       FUN=function(x, y, bg, fg, size, lwd) {
                             symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd,
                                                      circles=size, add=TRUE, inches=FALSE)
                        })
  }


#' Convert HSV Color values to Hex Color
#'
#' This function supports conversion of colors from the HSV scale to Hex scale. This function is a data.frame specific helper (wrapper) function that uses the built-in hsv function under the hood.
#' @param takes a row number (of the data frame that contains Hue, Saturation, and Value in columns 6, 7, and 8 respectively) as input.
#' @keywords color, hex, hsv
#' @export
#' @examples
#' hsvtohex()
hsvtohex <- function(k, d) { 
  return(hsv(h = d[k,6], s = d[k,7], v=d[k,8])) 
  }

#' Generate Three Panel (Node) Graph Display
#'
#' This function generates a 3-panel visualization of the same graph. The first graph (left) depicts the small-world graph itself. The second graph (middle) depicts the same graph from the left, but with predicted noticeability scores overlayed on top. The third graph (right) displays a heatmap visualization to help illustrate the data from the second (middle) graph. This function focuses on node data only. The related generateNewValidationEdges functions provides similar functionality for edges in graphs.
#' @param TRUE/FALSE boolean toggling paneled display. Defaults to T.
#' @keywords graph, visual, noticeability, saliency, heatmap
#' @export
#' @examples
#' generateNewValidation()
generateNewValidation <- function(paneled = T) {
  
  smarg <- barabasi.game(10, directed = F)
  csmarg <- smarg
  # selected ~ nodeshape + network + nodeheight + numConnected + 
  # nodeborderwidth + nodeHue + nodeSaturation + nodeValue
  rptdf <- cbind(sample(15:30, 10, replace = T), #nodeheight
                 rep(0, 10),     
                 degree(csmarg), #numConnected
                 rep(4, 10),     #numNodes
                 sample(c(1.5:5), 10, replace = T), #nodeborderwidth
                 sample(1:100, 10) / 100, #nodeHue
                 sample(1:100, 10) / 100, #nodeSaturation
                 sample(1:100, 10) / 100) #nodeValue
  ptc <- expd.nodes.1[5,]
  
  scores <- vector();
  for (i in 1:10) {
    ptc[,3:10] <- rptdf[i,]
    scores <- append(scores, predict(rf1.nodes.1, ptc))
  }
  
  
  for (k in unique(rptdf[,5] / max(rptdf[,5]))) {
    add.vertex.shape(paste("fcircle",k,sep=''), clip=vertex.shapes("circle")$clip,
                     plot=mycircle, parameters=list(vertex.frame.color="black",
                                                    vertex.frame.width=k))
  }
  
  # Layout
  lay <- layout.auto(csmarg)
  
  if (paneled) {
    dev.off()
    par(mfrow=c(1,3))
    par(mfrow=c(1,2))
    # Template
    V(csmarg)$label <- NA
    V(csmarg)$size <- rptdf[,1]
    V(csmarg)$color <- unlist(lapply(1:10, FUN=hsvtohex, d=rptdf))
    #plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape=function(x) paste("fcircle",k,sep=''), layout = lay)
    
    # With Score
    # V(csmarg)$label <- round(scores, 2) 
    # V(csmarg)$size <- rptdf[,1]
    # V(csmarg)$color <- unlist(lapply(1:10, FUN=hsvtohex))
    # plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape=function(x) paste("fcircle",k,sep=''), layout = lay)
    
    # With Heatmap
    V(csmarg)$label <- NA
    V(csmarg)$size <- rptdf[,1]
    V(csmarg)$color <- brewer.pal(9, "Reds")[round(scores*10, 0)]
    #plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape=function(x) paste("fcircle",k,sep=''), layout = lay)
  }
  else {
    dev.off()
    # With Score
    V(csmarg)$label <- round(scores, 2) 
    V(csmarg)$size <- rptdf[,1]
    #V(csmarg)$shape <- paste("fcircle",rptdf[,5],sep='')
    V(csmarg)$color <- unlist(lapply(1:10, FUN=hsvtohex, d=rptdf))
    # plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape=function(x) paste("fcircle",k,sep=''), layout = lay)
    
    # With Heatmap
    V(csmarg)$label <- NA
    V(csmarg)$size <- rptdf[,1]
    V(csmarg)$color <- brewer.pal(9, "Reds")[round(scores*10, 0)]
    # plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape=function(x) paste("fcircle",k,sep=''), layout = lay)
    
    # Template
    V(csmarg)$size <- rptdf[,1]
    V(csmarg)$color <- unlist(lapply(1:10, FUN=hsvtohex, d=rptdf))
    # plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape=function(x) paste("fcircle",k,sep=''), layout = lay)
  }
  
}


# Now for edges
pte <- expd.edges.1[1,]
hsvtohexedges <- function(k, d) {
  return(hsv(h = d[k,3], s = d[k,4], v=d[k,5])) 
  }

#' Generate Three Panel (Edge) Graph Display
#'
#' This function generates a 3-panel visualization of the same graph. The first graph (left) depicts the small-world graph itself. The second graph (middle) depicts the same graph from the left, but with predicted noticeability scores overlayed on top. The third graph (right) displays a heatmap visualization to help illustrate the data from the second (middle) graph. This function focuses on edge data only. The related generateNewValidation function provides similar functionality for nodes in graphs.
#' @param TRUE/FALSE boolean toggling paneled display. Defaults to T.
#' @keywords graph, visual, noticeability, saliency, heatmap
#' @export
#' @examples
#' generateNewValidation()
generateNewValidationEdges <- function(paneled = T) {
  
  smarg <- barabasi.game(10, directed = F)
  csmarg <- smarg
  numEdges <- length(E(csmarg))
  rptdf <- cbind(sample(2:8, numEdges, replace = T), sample(factor(expd.edges.1[,4]), numEdges, replace=T), sample(1:100, numEdges) / 100, sample(1:100, numEdges) / 100, sample(1:100, numEdges) / 100)
  
  scores <- vector();
  for (i in 1:numEdges) {
    pte[,3] <- rptdf[i,3]
    pte[,5:7] <- rptdf[i,3:5]
    scores <- append(scores, predict(rf1.edges.1, pte))
  }
  
  # Layout
  lay <- layout.auto(csmarg)
  
  if (paneled) {
    # par(mfrow=c(1,3))
    dev.off()
    par(mfrow=c(1,2))
    # Template
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- unlist(lapply(1:numEdges, FUN=hsvtohexedges, d=rptdf))
    V(csmarg)$label <- NA
    V(csmarg)$color <- "gray"
    plot(csmarg, layout = lay)
    
    # # With Score
    # E(csmarg)$label <- round(scores, 2) 
    # E(csmarg)$width <- rptdf[,1]
    # E(csmarg)$color <- unlist(lapply(1:numEdges, FUN=hsvtohexedges))
    # plot(csmarg, layout = lay)
    
    # With Heatmap
    E(csmarg)$label <- NA
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- brewer.pal(9, "Reds")[rank(scores)]
    V(csmarg)$label <- NA
    V(csmarg)$color <- "gray"
    plot(csmarg, layout = lay)
  }
  else {
    dev.off()
    # With Score
    E(csmarg)$label <- round(scores, 2) 
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- unlist(lapply(1:numEdges, FUN=hsvtohexedges, d=rptdf))
    V(csmarg)$label <- NA
    V(csmarg)$color <- "gray"
    plot(csmarg, layout = lay)
    
    # With Heatmap
    E(csmarg)$label <- NA
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- brewer.pal(9, "Reds")[rank(scores)]
    V(csmarg)$label <- NA
    V(csmarg)$color <- "gray"
    plot(csmarg, layout = lay)
    
    # Template
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- unlist(lapply(1:numEdges, FUN=hsvtohexedges, d=rptdf))
    V(csmarg)$label <- NA
    V(csmarg)$color <- "gray"
    plot(csmarg, layout = lay)
  }
  
}



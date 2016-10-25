
library(igraph)
data(expd.edges.1)
data(expd.nodes.1)

#' Convert HSV Color values to Hex Color
#'
#' This function supports conversion of colors from the HSV scale to Hex scale. This function is a data.frame specific helper (wrapper) function that uses the built-in hsv function under the hood.
#' @param takes a row number (of the data frame that contains Hue, Saturation, and Value in columns 6, 7, and 8 respectively) as input.
#' @keywords color, hex, hsv
#' @export
#' @examples
#' hsvtohex()
hsvtohex <- function(k) { return(hsv(h = rptdf[k,6], s = rptdf[k,7], v=rptdf[k,8])) }

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
  rptdf <- cbind(sample(15:30, 10, replace = T), rep(0, 10), degree(csmarg), rep(4, 10), sample(c(1.5:5), 10, replace = T), sample(1:100, 10) / 100, sample(1:100, 10) / 100, sample(1:100, 10) / 100)
  
  scores <- vector();
  for (i in 1:10) {
    ptc[,3:10] <- rptdf[i,]
    scores <- append(scores, predict(rf1.nodes.1, ptc))
  }
  
  for (k in unique(rptdf[,5] / max(rptdf[,5]))) {
    add.vertex.shape(paste("fcircle",k,sep=''), clip=igraph.shape.noclip,
                     plot=mycircle, parameters=list(vertex.frame.color="black",
                                                    vertex.frame.width=k))
  }
  
  # Layout
  lay <- layout.auto(csmarg)
  
  if (paneled) {
    par(mfrow=c(1,3))
    # Template
    V(csmarg)$label <- NA
    V(csmarg)$size <- rptdf[,1]
    V(csmarg)$color <- unlist(lapply(1:10, FUN=hsvtohex))
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
    
    # With Score
    V(csmarg)$label <- round(scores, 2) 
    V(csmarg)$size <- rptdf[,1]
    #V(csmarg)$shape <- paste("fcircle",rptdf[,5],sep='')
    V(csmarg)$color <- unlist(lapply(1:10, FUN=hsvtohex))
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
    
    # With Heatmap
    V(csmarg)$label <- NA
    V(csmarg)$size <- rptdf[,1]
    V(csmarg)$color <- brewer.pal(9, "Reds")[round(scores*10, 0)]
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
  }
  else {
    dev.off()
    # With Score
    V(csmarg)$label <- round(scores, 2) 
    V(csmarg)$size <- rptdf[,1]
    #V(csmarg)$shape <- paste("fcircle",rptdf[,5],sep='')
    V(csmarg)$color <- unlist(lapply(1:10, FUN=hsvtohex))
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
    
    # With Heatmap
    V(csmarg)$label <- NA
    V(csmarg)$size <- rptdf[,1]
    V(csmarg)$color <- brewer.pal(9, "Reds")[round(scores*10, 0)]
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
    
    # Template
    V(csmarg)$size <- rptdf[,1]
    V(csmarg)$color <- unlist(lapply(1:10, FUN=hsvtohex))
    plot(csmarg, vertex.frame.width=rptdf[,5], vertex.shape="fcircle", layout = lay)
  }
  
}


# Now for edges
pte <- expd.edges.1[1,]
hsvtohexedges <- function(k) { return(hsv(h = rptdf[k,3], s = rptdf[k,4], v=rptdf[k,5])) }

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
    par(mfrow=c(1,3))
    # Template
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- unlist(lapply(1:numEdges, FUN=hsvtohexedges))
    plot(csmarg, layout = lay)
    
    # With Score
    E(csmarg)$label <- round(scores, 2) 
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- unlist(lapply(1:numEdges, FUN=hsvtohexedges))
    plot(csmarg, layout = lay)
    
    # With Heatmap
    E(csmarg)$label <- NA
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- brewer.pal(9, "Reds")[rank(scores)]
    plot(csmarg, layout = lay)
  }
  else {
    dev.off()
    # With Score
    E(csmarg)$label <- round(scores, 2) 
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- unlist(lapply(1:numEdges, FUN=hsvtohexedges))
    plot(csmarg, layout = lay)
    
    # With Heatmap
    E(csmarg)$label <- NA
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- brewer.pal(9, "Reds")[rank(scores)]
    plot(csmarg, layout = lay)
    
    # Template
    E(csmarg)$width <- rptdf[,1]
    E(csmarg)$color <- unlist(lapply(1:numEdges, FUN=hsvtohexedges))
    plot(csmarg, layout = lay)
  }
  
}

















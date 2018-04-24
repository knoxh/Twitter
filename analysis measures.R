source("rtweet.R")

# might need this
edgeList <- cbind(as.character(edgeList$source), as.character(edgeList$target))


# Girvan and Newman Algorithm network
edgebetweenness <- function(edgeList, originalTweeter){
eb <- edge.betweenness.community(g)
originalTweeterID <- originalTweeter$user_id
V(g)[as.character(originalTweeterID)]$color<-"black"
betweennessnetwork <- plot(eb, as.undirected(g), vertex.size = 10, vertex.label = NA, layout = layout.fruchterman.reingold(g))
return(betweennessnetwork)
}


closeness <- function(edgeList){
close <- closeness(g, mode="all")
fine = 500 # this will adjust the resolving power
pal = colorRampPalette(c('gray','blue', 'plum')) # color scheme
graphCol = pal(fine)[as.numeric(cut(close,breaks = fine))]
# now we plot it with those colors
closenetwork <- plot(as.undirected(g), vertex.color=graphCol, vertex.size = 10, vertex.label = NA, layout = layout.fruchterman.reingold(g))
return(closenetwork)
}

degreecentrality <- function(edgeList){
  degree <- degree(as.undirected(g))
  fine = 500 
  pal = colorRampPalette(c('gray','blue', 'plum')) 
  graphCol = pal(fine)[as.numeric(cut(degree,breaks = fine))]
  # now we plot it with those colors
  degreenetwork <- plot(as.undirected(g), vertex.color=graphCol, vertex.size = 10, vertex.label = NA, layout = layout.fruchterman.reingold(g))
  return (degreenetwork)
  }
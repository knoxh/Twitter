library(rtweet)
library(readr)
library(igraph)
library(dplyr)
library(rgexf)

make_status_directory <- function(path = getwd(), status_id) {
  directory <- paste0(path,"/statusId_",status_id)
  dir.create(directory)
  return(directory)
}

originalTweeter <- function(status_id){
  originalTweet <- lookup_statuses(status_id)
  originalTweeter <- data.frame(user_id = originalTweet$user_id)
  return(originalTweeter)
}

initializeFiles <- function(status_id, originalTweeter, directory) {
  
  who_retweet <- get_retweeters(status_id=status_id)
  who_retweet <- bind_rows(originalTweeter, who_retweet)
  
  originalTweeterFile <- paste0(directory, "/originalTweeter.txt")
  write.table(originalTweeter, row.names = FALSE, file = originalTweeterFile)
  
  # may not need
  retweetersFile <- paste0(directory, "/retweeters.txt")
  write.table(who_retweet, row.names = FALSE, file = retweetersFile)
  
  return(who_retweet)
}

# get the retweeters from "/statusId_XXXX/userId_XXXX.txt"
getRetweeters <- function(status_id, directory) {
  
  # read all the userId files and get the userIds only, and return it
  searchStr <- paste0(directory, "/userId*")
  files <- Sys.glob(searchStr)
  retweeters <- strsplit(files, "userId_")
  retweeters <- gsub(".txt", "", sapply(retweeters, function(x) x[[2]]))
  return(retweeters)
}


getFollowers <- function(who_retweet, retweeters, directory){
  retweetersID <- who_retweet$user_id
  
  newRetweeters <- setdiff(retweetersID, retweeters)
  
  cat("identified ", length(newRetweeters), " new retweeters\n")
  
  for (i in 1:length(newRetweeters)){
    retweeter <- as.character(newRetweeters[i])
    followers <- get_followers(retweeter, retryonratelimit = T)
    fileName <- paste0(directory, "/userId_", retweeter, ".txt")
    write.table(followers, file = fileName, row.names = FALSE) 
    limits <- rate_limit("followers/ids")
    cat(nrow(followers))
  }
  return(retweetersID)
}
  
# edge list function
createEdgeList <- function(retweetersID){
  
  # intersect the files and the list of retweeters
  for (i in 1:length(retweetersID)){
    followers <- read_csv(files[i], col_types = cols(user_id = col_character()))
    common <- intersect(followers$user_id, retweetersID)
    
    # edge list
    if (length(common) >0) {
      e <- cbind(retweetersID[i], common)
      edgeList <- rbind(edgeList, e)
    }
    colnames(edgeList) <- c("source","target")
  } 
  return(edgeList)
}

graph <- function(edgeList){
  g <- graph.edgelist(edgeList, directed = FALSE)
  V(g)$color <- "black"
  V(g)[originalTweeter$user_id]$color <- "red"
  graph <- plot(g)
  return(graph)
}
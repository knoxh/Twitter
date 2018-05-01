library(rtweet)
library(readr)
library(igraph)
library(dplyr)
library(rgexf)

# search for specfic tweet

rt <- function(search){
  searchtext <- search$text
  retweets_text = grep("RT|via)((?:\\b\\W*@\\w+)+)", searchtext, ignore.case = TRUE)
  return(retweets_text)
}

non_retweets <- function(search, retweets_text){
  non_retweets <- setdiff(1:nrow(search), retweets_text)
  return(non_retweets)
}

weird_tweets <- function(search, non_retweets){
  q <- search$is_quote[non_retweets]
  t <- cbind(q, non_retweets)
  weird_tweets = vector(mode="character", length=0)
  quote = vector(mode="character", length=0)
  for(i in 1:length(q)){
    if(q[i]==FALSE){
      weird_tweets <- append(weird_tweets, as.character(t[i,2]))
    }
  }
  return(weird_tweets)
}

rtweetid <- function(search, retweets_text){
  who_retweet <- as.list(search$user_id[retweets_text])
  # want to add the quotes too, but it won't work
  #quote <- setdiff(non_retweets,weird_tweets)
  #who_retweet <- append(rtweetid, quote)
  return(who_retweet)
  }

###################################################################

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
    
    ## get the number of followers for each retweeter
    ## if user has >= 75000, display message stating they have more than
    ## 75,000 followers
    
    followers <- get_followers(retweeter, n= 75000, retryonratelimit = TRUE)
    fileName <- paste0(directory, "/userId_", retweeter, ".txt")
    write.table(followers, file = fileName, row.names = FALSE) 
    limits <- rate_limit("followers/ids")
    cat("Number of followers for: ", retweeter, nrow(followers), "  ")
    follow_count <- function(retweeter){return(lookup_users(retweeter)$followers_count)}
    if(follow_count(retweeter) > 75000){
      cat("User has more than 75000 followers")}
  }
  return(retweetersID)
}

# edge list function
createEdgeList <- function(directory, retweetersID){
  searchStr <- paste0(directory, "/userId*")
  files <- Sys.glob(searchStr)
  
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

graph <- function(edgeList, originalTweeter){                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
  g <- graph_from_edgelist(edgeList)
  V(g)$color <- "black"
  originalTweeterID <- originalTweeter$user_id
  V(g)[as.character(originalTweeterID)]$color <- "blue"
  graph <- plot(as.undirected(g), vertex.size = 6, vertex.label = NA, edge.width = 1, layout = layout.fruchterman.reingold(g))
  return(graph)
}
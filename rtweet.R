library(rtweet)
library(readr)
library(igraph)
library(dplyr)


status_id <- "975714756064575489"

# calling functions - are the arguments correct?

who_retweet <- initializeFiles(status_id)
newFollowers <- getFollowers(who_retweet)
edgeList <- createEdgeList(newRetweeters)


# FUNCTIONS

initializeFiles <- function(status_id) {
  # get retweeters and create directory if it does not exist
  # FIX: check if directory exists
  originalTweet <- lookup_statuses(status_id)
  originalTweeter <- data.frame(user_id = originalTweet$user_id)
  
  who_retweet <- get_retweeters(status_id=status_id)
  who_retweet <- bind_rows(originalTweeter, who_retweet)
  directory <- paste0("statusId_", status_id)
  dir.create(directory)
  
  originalTweeterFile <- paste0(directory, "/originalTweeter.txt")
  write.table(originalTweeter, row.names = FALSE, file = originalTweeterFile)
  
  # may not need
  retweetersFile <- paste0(directory, "/retweeters.txt")
  write.table(who_retweet, row.names = FALSE, file = retweetersFile)
  
  return(who_retweet)
}

# does directory have to be a parameter for this function?
getFollowers <- function(who_retweet, directory){
  
  searchStr <- paste0(directory, "/userId*")
  files <- Sys.glob(searchStr)
  
  # vector of retweeter IDs
  retweetersID <- c()
  for(i in 1:nrow(who_retweet)){
    retweetersID <- append(retweetersID, gsub(".txt", "", strsplit(files[i], "userId_")[[1]][2]))
  }
  
  fileIDs <- c()
  for(i in 1:length(files)){
    fileIDs <- append(fileIDs, gsub(".txt", "", strsplit(files[i], "userId_")[[1]][2]))
  }
  
  # find out who doesn't already have a file of followers
  # I don't think i did this right - should newRetweeters be in the first function before new files are created?
  newRetweeters <- setdiff(retweetersID, fileIDs)
 
  for (i in 1:nrow(newRetweeters)){
    retweeter <- as.character(newRetweeters$user_id[i])
    followers <- get_followers(retweeter, retryonratelimit = T)
    fileName <- paste0(directory, "/userId_", retweeter, ".txt")
    write.table(followers, file = fileName, row.names = FALSE) 
    limits <- rate_limit("followers/ids")
    cat(nrow(followers))
  }
  
  
  return(newRetweeters)
}
  

# edge list function
createEdgeList <- function(newRetweeters){
  searchStr <- paste0(directory, "/userId*")
  files <- Sys.glob(searchStr)
  
  # vector of retweeter IDs
  retweetersID <- c()
  for(i in 1:nrow(who_retweet)){
    retweetersID <- append(retweetersID, gsub(".txt", "", strsplit(files[i], "userId_")[[1]][2]))
  }
  
  # intersect the files and the list of retweeters
  for (i in 1:length(retweetersID)){
    followers <- read_csv(files[i], col_types = cols(user_id = col_character()))
    common <- intersect(followers$user_id, retweetersID)
    
    # edge list
    if (length(common) >0) {
      e <- cbind(retweetersID[i], common)
      edgeList <- rbind(edgeList, e)
    }
  } 
  return(edgeList)
}
g <- graph_from_edgelist(edgeList)
g2 <- graph.edgelist(edgeList, directed = FALSE)
plot(g2)


##############################


# Convert followers to a list of ID's of the followers
followers_list = list()
for(j in 1:nrow(followers)){
  followers_list[j] <- as.list(followers$user_id)[j]
}

# see if any of the retweeters follow each other
who_retweet_list <- as.list(who_retweet[,1])
common <- intersect(who_retweet_list, followers_list)
cat(common)
cat('\n')
cat("common followers: ")

# edge list
if (length(common) >0) {
  e <- cbind(user$user_id, common)
  edgeList <- rbind(edgeList, e)
}

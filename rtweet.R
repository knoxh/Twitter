library(rtweet)
library(readr)
library(igraph)


status_id <- "962217906242293760"
originalTweet <- lookup_statuses(status_id)
originalTweeter <- data.frame(user_id = originalTweet$user_id)

who_retweet <- get_retweeters(status_id=status_id, n=100)
who_retweet$user_id <- c(who_retweet$user_id, originalUser)


directory <- paste0("statusId_", status_id)

dir.create(directory)

retweetersFile <- paste0(directory, "/retweeters.txt")
write.table(who_retweet, row.names = FALSE, file = retweetersFile)


originalTweeterFile <- paste0(directory, "/originalTweeter.txt")
write.table(originalTweeter, row.names = FALSE, file = originalTweeterFile)


# make files of followers for 100 retweeters
for (i in 1:100){
  # get retweeter ID 
  retweeter <- as.character(who_retweet$user_id)[i]

  followers <- get_followers(retweeter, n=4000, retryonratelimit = T)
  fileName <- paste0(directory, "/userId_", retweeter, ".txt")
  write.table(followers, file = fileName, row.names = FALSE) 
  
  limits <- rate_limit("followers/ids")
  cat(nrow(followers))
}



# Assignment 1: read in retweeters (done) and follower information from
# the current directory to create the edge list

retweeters <- read_csv(retweetersFile, col_types = cols(user_id = col_character()))

searchStr <- paste0(directory, "/userId*")
files <- Sys.glob(searchStr)

# vector of retweeter IDs
retweetersID <- c()
for(i in 1:94){
retweetersID <- append(retweetersID, gsub(".txt", "", strsplit(files[i], "userId_")[[1]][2]))
}


# intersect the files and the list of retweeters
for (i in 1:94){
  followers <- read_csv(files[i], col_types = cols(user_id = col_character()))
  common <- intersect(followers$user_id, retweetersID)
  
  # edge list
  if (length(common) >0) {
    e <- cbind(retweetersID[i], common)
    edgeList <- rbind(edgeList, e)
  }
} 

# graph from edge list
g <- graph_from_edgelist(edgeList)
plot(g, layout = layout_with_fr, vertex.label = NA)

who_retweet <- initializeFiles(status_id)
newFollowers <- getFollowers(who_retweet)
edgeList <- createEdgeList(newRetweeters)


# Assignment 2 (harder): write function 

initializeFiles <- function(status_id) {
  # get retweeters and create directory if it does not exist
  # FIX: check if directory exists
  originalTweet <- lookup_statuses(status_id)
  originalTweeter <- data.frame(user_id = originalTweet$user_id)
  
  who_retweet <- get_retweeters(status_id=status_id)
  who_retweet$user_id <- c(who_retweet$user_id, originalTweeter)
  directory <- paste0("statusId_", status_id)
  dir.create(directory)
  
  originalTweeterFile <- paste0(directory, "/originalTweeter.txt")
  write.table(originalTweeter, row.names = FALSE, file = originalTweeterFile)
  
  retweetersFile <- paste0(directory, "/retweeters.txt")
  write.table(who_retweet, row.names = FALSE, file = retweetersFile)
  return(who_retweet)
}


getFollowers <- function(who_retweet){
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
  newRetweeters <- setdiff(retweetersID, fileIDs)
 
  for (i in 1:nrow(length(newRetweeters))){
    retweeter <- as.character(newRetweeters[i])
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
  for(i in 1:newRetweeters[i]){
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
q()

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

#############################################################################

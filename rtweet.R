library(rtweet)
library(readr)
library(igraph)

status_id <- "962217906242293760"

who_retweet <- get_retweeters(status_id=status_id, n=100)

directory <- paste0("statusId_", status_id)

dir.create(directory)

retweetersFile <- paste0(directory, "/retweeters.txt")

write.table(who_retweet, row.names = FALSE, file = retweetersFile)


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


# Assignment 2 (harder): write function 

getFollowers <- function(status_id, completed = "") {
  # get retweeters and create directory if it does not exist
  # FIX: check if directory exists
  who_retweet <- get_retweeters(status_id=status_id)
  directory <- paste0("statusId_", status_id)
  dir.create(directory)
  retweetersFile <- paste0(directory, "/retweeters.txt")
  write.table(who_retweet, row.names = FALSE, file = retweetersFile)
  
  # get followers of retweeters and put in files
  # FIX: should skip over retweeters where followers are already determined
  for (i in 1:nrow(who_retweet)){
    retweeter <- as.character(who_retweet$user_id)[i]
    followers <- get_followers(retweeter, retryonratelimit = T)
    fileName <- paste0(directory, "/userId_", retweeter, ".txt")
    write.table(followers, file = fileName, row.names = FALSE) 
    limits <- rate_limit("followers/ids")
    cat(nrow(followers))
  }
}


# edge list function
createEdgeList <- function(status_id, createDirectory){
  searchStr <- paste0(directory, "/userId*")
  files <- Sys.glob(searchStr)
  
  # vector of retweeter IDs
  retweetersID <- c()
  for(i in 1:nrow(who_retweet)){
    retweetersID <- append(retweetersID, gsub(".txt", "", strsplit(files[i], "userId_")[[1]][2]))
  }
  
  # intersect the files and the list of retweeters
  for (i in 1:nrow(who_retweet)){
    followers <- read_csv(files[i], col_types = cols(user_id = col_character()))
    common <- intersect(followers$user_id, retweetersID)
    
    # edge list
    if (length(common) >0) {
      e <- cbind(retweetersID[i], common)
      edgeList <- rbind(edgeList, e)
    }
  } 
}


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

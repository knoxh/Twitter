
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
View(retweeters)

searchStr <- paste0(directory, "/userId*")
files <- Sys.glob(searchStr)

# get a vector of retweeters from the files

followers <- read_csv(files[1], col_types = cols(user_id = col_character()))

# vector with retweeter IDs
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



alreadyHave <- strsplit(files, "userId_")
alreadyHave <- sapply(alreadyHave, function(x)x[[2]])
alreadyHave <- gsub(".txt", "", alreadyHave)
need <- setdiff(retweeters$user_id, alreadyHave)

# have to write function
getFollowers <- function(status_id, completed = "") {
  
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

cat(common)
cat('\n')common <- intersect(who_retweet_list, followers_list)
cat("common followers: ")

# edge list
if (length(common) >0) {
  e <- cbind(user$user_id, common)
  edgeList <- rbind(edgeList, e)
}

#############################################################################

tweetstext <- tweetsdf$text

# READ ME: length=200, but I searched for 180 tweets? Is it a data frame?
length(tweetstext)

# READ ME: Can use lines 19-28 instead of 30-50?

who_retweet = list()
who_post = list()
# Do i have to specify the element of the list? Is an if-else statement not the best?

index <- tweetsdf$is_retweet
who_retweet <- tweetsdf$screen_name[index]
who_post <- tweetsdf$screen_name[!index]



#################################################


who_retweet = unlist(who_retweet)
userList <- lookup_users(who_retweet)

# get id's of the retweeters
who_retweet_id = list()
for(i in 1:length(userList))
{
  who_retweet_id[[i]] = userList[i,1]$user_id
}
user_follower_ids=list()

# see who is in common between the retweeters and their followers
# returning the wrong number of followers
index <- 1:25
edgeList <- NULL
for (i in index){
  
  cat("User number ", i, "\n")
  user <- who_retweet_id[[i]]
  user_follower_ids[[i]] <- get_followers(user, n=20, retryonratelimit = TRUE)
}
  common <- intersect(who_retweet_id, user_follower_ids)
  cat("common followers: ")
  cat(common)
  cat('\n')
  
  if (length(common) >0) {
    e <- cbind(user$user_id, common)
    edgeList <- rbind(edgeList, e)
  }
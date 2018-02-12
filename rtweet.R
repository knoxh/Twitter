install.packages("rtweet")
library(rtweet)


who_retweet <- get_retweeters(status_id="962217906242293760", n=100)


# TO DO: for each retweeter 

for (i in 1:100){
  # get retweeter ID 
  retweeter <- as.character(who_retweet$user_id)[i]

  followers <- tryCatch(get_followers(retweeter, n=4000, retryonratelimit = T))
  
  limits <- rate_limit("followers/ids")
  cat(nrow(followers))
}

# Convert followers to a list of ID's of the followers
followers_list = list()
for(j in 1:nrow(followers)){
  followers_list[j] <- as.list(followers$user_id)[j]
}

# see if any of the retweeters follow each other
who_retweet_list <- as.list(who_retweet[,1])
common <- intersect(who_retweet_list, followers_list)
cat("common followers: ")
cat(common)
cat('\n')

# edge list
if (length(common) >0) {
  e <- cbind(user$user_id, common)
  edgeList <- rbind(edgeList, e)
}

#############################################################################

tweetstext <- tweetsdf$text
#for (i in 1:nrow(tweetsdf)) {
#  tweetstext[[i]] <- tweetsdf[i,5]
#}

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
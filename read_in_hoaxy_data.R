# This is for extracting the Twitter data from Hoaxy
# This is just a different format. I didn't change any functionality from the other script.
source("rtweet_authenticate.R")

library(rtweet)
library(readr)
library(igraph)
#install.packages('dplyr', dependencies = TRUE)
library(dplyr)
library(rgexf)
# make sure readr and Rcpp are installed with dependencies set to true
# install.packages('readr', dependencies = TRUE)
library(Rcpp)
library(ggplot2)

# disable scientific notation -- messing things up
options(scipen=999)

# READ ME: Need to change the directory when switching computers
# read in table from Hoaxy 
#directory <- dir.create("/Users/c90649/Documents/Random")
#setwd("/Users/c90649/Documents/Random")
directory <- getwd()

#dont need this if using the import dataset button
#tbl <- read.table("crashed_train_data.csv", sep=",", header = T)
tbl <- dogs_hoaxy_data

# take out unnecessary columns
tbl$domain=NULL
tbl$id=NULL
tbl$site_domain=NULL
tbl$title=NULL
tbl$url_id=NULL
tbl$url_raw=NULL

# get retweets from the hoaxy table 
non_retweets = list()
for(row in 1:nrow(tbl)){
  if(tbl[row,]$tweet_type != "retweet"){
    non_retweets <- append(non_retweets, row)
  }
}

# has all of the data of tweets that are retweets
non_retweets <- unlist(non_retweets)
retweet_data = tbl[-non_retweets, ]
rownames(retweet_data) = NULL

# has all of the data of tweets that are not retweets, e.g., mentions
non_retweet_data = tbl[non_retweets, ]

# create edge list
# using twitter ID's -- check if ID's are easier than username, the length of the ID's might be too long
source <- unlist(tbl$from_user_screen_name)
target <- unlist(tbl$to_user_screen_name)

edge_list <- cbind(source, target)

# how many different original tweeters are there? -- interesting to compare how may hubs there are to this number
length(unique(source))

graph <- function(edgelist){                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
  g <- graph_from_edgelist(edgelist)
  V(g)$color <- "black"
  #originalTweeterID <- originalTweeter$user_id
  #V(g)[as.character(originalTweeterID)]$color <- "blue"
  graph <- plot(as.undirected(g), vertex.size = 6, vertex.label = NA, edge.width = 1, layout = layout.fruchterman.reingold(g))
  return(graph)
}
# graph the network
g <- graph_from_edgelist((edge_list))
network <- graph(edge_list)

# remove multiple edges -- do we want to look at weights?
sg <- simplify(g)
graph(as_edgelist(sg))

# make degree distribution
sdegree <- degree(sg, mode="all", normalized=F)
sg_degree_histogram <- as.data.frame(table(sdegree))

ggplot(sg_degree_histogram, aes(x=sdegree, y=Freq)) +
  geom_point()

# number of different tweets that are in our data
# maybe compare the retweeters of these tweets to what data we have
diff_tweets <- unique(tbl$tweet_id)

# FIX ME: currently returning empty -- these are retweets so they probably
# didn't get retweeted
for(i in 1:length(diff_tweets)){
  print(get_retweeters(status_id = as.character(diff_tweets[[i]])))
}

# the user id's of all users in the data set. i.e., the id's of the nodes
vertex_ids <- union(tbl$from_user_id, tbl$to_user_id)

getFollowers <- function(vertex_ids, directory) {
  for(i in 1:length(vertex_ids)){
    
    # dont want to get the followers of a user if we already have their followers
    # waste of time
    searchStr <- paste0(directory, "/userId*")
    files <- Sys.glob(searchStr)
    file_made <- strsplit(files, "userId_")
    file_made <- gsub(".txt", "", sapply(file_made, function(x) x[[2]]))
    
    newRetweeters <- setdiff(vertex_ids, file_made)
    
    retweeter <- newRetweeters[[i]]
    
    ## get the number of followers for each retweeter
    ## if user has >= 75000, display message stating they have more than
    ## 75,000 followers
    
    # will return NULL if the user is private -- definitely a setback
    followers <- tryCatch(get_followers(retweeter, n=75000, retryonratelimit = TRUE), error = function(c) NULL)
    fileName <- paste0(directory, "/userId_", as.character(retweeter), ".txt")
    write.table(followers, file = fileName, row.names = FALSE) 
    limits <- rate_limit("followers/ids")
    cat("Number of followers for: ", retweeter, nrow(followers), "  ")
    
    follow_count <- function(retweeter){
      r <- lookup_users(retweeter)$followers_count
      return(r)
    }
    
}
    
    #if(follow_count(retweeter) > 75000){
    #  cat("User has more than 75000 followers")
    #}
  return(follow_count(vertex_ids))
}

getFollowers(vertex_ids, directory)

# FIX ME: Only returning 14 users, when it should be 35. When I try to look up the users on Twitter, 
# the ones that are not returning anything are nonexistent.
# Looked up the problem, and the problem occurs when you try to get friends of protected user. Results will be very skewed.
lookup_users(vertex_ids)$followers_count

# read in the files of followers and create the edge list for the bigger/dual-layer network
createEdgeList <- function(directory, vertex_ids){
  searchStr <- paste0(directory, "/userId_", vertex_ids, ".txt")
  files <- Sys.glob(searchStr)
  
  edgeList <- data.frame()
  # intersect the files and the list of retweeters
  for (i in 1:length(files)){
    # some of the users are private, so they don't have a file 
    # silent=TRUE allows the code to keep running even if the file doesn't exist
    followers <- try(read_csv(searchStr[1], col_types = cols(user_id = col_character())), silent=TRUE)
    common <- try(intersect(followers$user_id, as.character(vertex_ids)), silent=TRUE)
    
    # edge list
    if (length(common) >0) {
      # should be the id in the searchStr element
      e <- cbind(vertex_ids[i], common)
      edgeList <- rbind(edgeList, e)
    }
     colnames(edgeList) <- c("source","target")
  } 
  return(edgeList)
}

# Error, because the edge list is empty. Double-checked. Not sure if that's true.
# Try different data sets.
edgeList <- createEdgeList(directory, vertex_ids)

# FIX ME: figure out why there are so many NA's
edgeList <- na.omit(edgeList)

# graph the network from the edge list
graph <- function(edgeList){                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
  g <- graph_from_edgelist(edgeList)
  V(g)$color <- "black"
  graph <- plot(as.undirected(g), vertex.size = 6, vertex.label = NA, edge.width = 1, layout = layout.fruchterman.reingold(g))
  return(graph)
}

# graphs how users actually saw the tweet
graph(as.matrix(edgeList))

######################################################################
# Text Mining
library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)

florence <- read.csv("~/ECSU/Fall 2017/Thesis Proposal/Hoaxy Data/florence.csv")

removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

removeWords(str, stopwords)
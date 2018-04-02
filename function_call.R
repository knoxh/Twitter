source("rtweet.R")
status_id <- "980584845465341952"

# calling functions 
directory <- make_status_directory(path=getwd(), status_id)
originalTweeter <- originalTweeter(status_id)
who_retweet <- initializeFiles(status_id, originalTweeter, directory)
retweeters <- getRetweeters(status_id, directory)
retweetersID <- getFollowers(who_retweet, status_id)
edgeList <- createEdgeList(retweetersID)
network <- graph(edgeList)

# write csv for gephi
write.csv(edgeList, file=status_id)
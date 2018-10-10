library(dplyr)
library(tidytext)
library(stringr)
library(sentimentr)
library(tm)

# Comparing the base sentiment function to the different tidy dictionaries
# The base sentiment function appears to be a better measure

# data frame of example sentences
dd <- data.frame(final_clean = c("i like you", "i like like like you", "i love you", 
                                 "i do not love you", "i don't love you"))

# make sure they are characters
dd$final_clean <- as.character(dd$final_clean)

# bing sentiment from tidy (negative or positive)
for(i in 1:nrow(dd)){
  w <- select(dd[i,,drop=FALSE], final_clean) %>% unnest_tokens(word, final_clean)
  
  print(w %>% inner_join(get_sentiments("bing")))
}

# AFINN sentiment from tidy (-5 to 5)
for(i in 1:nrow(dd)){
  w <- select(dd[i,,drop=FALSE], final_clean) %>% unnest_tokens(word, final_clean)
  
  print(w %>% inner_join(get_sentiments("afinn")))
}
# it counts "like like like" as three different instances
# love is more positive than like


# sentiment of the sentences
# sentiment of sentence is not the sum of the sentiments of the words - smarter 
lapply(dd, function(x) sentiment(x))

# sentiment of the words 
# split the words
separate <- function(sentence){
  words <- unlist(strsplit(sentence, " "))
  return(words)
}

# gets sentiment of each word
l <- list()
for(i in 1:nrow(dd)){
  s <- lapply(lapply(dd[i,], separate), sentiment)
  l[[i]] <- as.vector(data.frame(s)$sentiment)
}

l
### Extract Twitter Data 
## Functions

# Clean Function
clean <- function(string){
  vec <- tolower(string)
  vec <- substring(vec, seq(1, nchar(vec)-1), seq(2, nchar(vec)))
  vec <- vec[vec %in% myletterpairs]
  return(vec)
}

# Freq function
freq <- function(vec) {
  ff <- sapply(myletterpairs, FUN = function(x){mean(vec==x)})
  ff[is.na(ff)] <- 0
  return(ff)
}

# Features Function
features <- function(text, language) {
  vec <- freq(text)
  return(cbind(language, as.data.frame(t(vec))))
}

# Load Library
library(httr)
library(twitteR)


# Connect to Twitter
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_token_secret)
# Select no and then 0 (not sure what these questions are asking but it works!)

# Extract tweets
tweets <- searchTwitter(' ', n=1000, lan = 'en') 
tweets.text <- sapply(tweets, function(x) x$getText())


# Clean Tweets
# Emojis get displayed as something called unicode so this removes that
tweets.clean <- iconv(tweets.text, 'ASCII', 'UTF-8', sub = '') 
tweets.clean <- sapply(tweets.clean, clean)

# Create frequencies and put into dataframe 
t <- t(sapply(tweets.clean, function(x) freq(x)))
rownames(t) <- NULL
t <- data.frame(t)
t$language <- 1

write.csv(t, 'tweets.csv', row.names = FALSE)

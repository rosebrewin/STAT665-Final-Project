library(tm.plugin.webmining)
library(tm)
id <- 'ec15c3069f8fa25c97e2ea10711e2042:17:71996751'

# Extract NYTimes Info
B <- WebCorpus(NYTimesSource('6 Baltimore Police Officers Charged', appid = id))
M <- WebCorpus(NYTimesSource('Murder Charge Woman',n =5, count =5, appid = id))
C <- WebCorpus(NYTimesSource('2 Indicted in George Washington', n = 5,count = 5, appid = id))
O <- WebCorpus(NYTimesSource('Ideology and Integrity',n = 5, count = 5, appid = id))
N <- WebCorpus(NYTimesSource('Nepal',n = 5, count =5, appid = id))
Mic <- WebCorpus(NYTimesSource('Microsoft', appid = id))
Tr <- WebCorpus(NYTimesSource('Travel', appid = id))



# Function that returns dataframe for each article
nytimes <- function(article) {
  mic1 <- unlist(article)
  text <- unname(unlist(strsplit(mic1, '.', fixed = TRUE)))
  text.clean <- sapply(text, clean)
  text.freq <- t(sapply(text.clean, freq))
  rownames(text.freq) <- NULL
  t <- data.frame(text.freq)
  return(t)
}


these <- c(B[[1]][1], M[[1]][1], C[[1]][1], O[[1]][1], N[[1]][1], Mic[[1]][1], Tr[[1]][1])
df <- NULL
for (i in 1:length(these)) {
  bb <- nytimes(these[i])
  df <- rbind(df,bb)
  print(these[i])
}


# Round Two
Ba <- WebCorpus(NYTimesSource('Baby',n = 5, count =5, appid = id))
WK <- WebCorpus(NYTimesSource('Kate Birth',n = 5, count =5, appid = id))
K <- WebCorpus(NYTimesSource('Cherry King',n = 5, count =5, appid = id))
Sc <- WebCorpus(NYTimesSource('Secret Warrior',n = 5, count =5, appid = id))
Ch <- WebCorpus(NYTimesSource('Why Honoring Charlie',n = 5, count =5, appid = id))
St <- WebCorpus(NYTimesSource('Student Loan Facts',n = 5, count =5, appid = id))


these2 <- c(Ba[[1]][1], WK[[1]][1], K[[1]][[1]], Sc[[1]][1], Ch[[1]][1], St[[1]][1])
for (i in 1:length(these2)) {
  bb <- nytimes(these2[i])
  df <- rbind(df,bb)
}

An <- WebCorpus(NYTimesSource('Hilary Clinton Courts',n = 5, count =5, appid = id))

bb <- nytimes(An[[1]][1])
df <- rbind(df, bb)
df2 <- df[1:1000,]


write.csv(df, 'nytimes.csv', row.names = FALSE)

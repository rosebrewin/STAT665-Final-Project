###########################################################
############## Language Classification ####################
###########################################################

### Set up
Sys.setlocale('LC_ALL','C')
myletters <- c(letters, " ")
myletterpairs <- matrix(nrow=length(myletters), ncol=length(myletters))
for (i in 1:length(myletters)) {
  for (j in 1:length(myletters)) {
    myletterpairs[j, i] <- paste(myletters[i], myletters[j], sep="")
  }
}
myletterpairs <- as.vector(myletterpairs)

clean <- function(string){
  vec <- tolower(string)
  vec <- substring(vec, seq(1, nchar(vec)-1), seq(2, nchar(vec)))
  vec <- vec[vec %in% myletterpairs]
  return(vec)
}

freq <- function(vec) {
  ff <- sapply(myletterpairs, FUN = function(x){mean(vec==x)})
  ff[is.na(ff)] <- 0
  return(ff)
}

features <- function(text, language) {
  vec <- freq(text)
  return(cbind(language, as.data.frame(t(vec))))
}

###########################################################
################### English Bible #########################
###########################################################

EngBible <- scan("http://www.gutenberg.org/cache/epub/10/pg10.txt", what="character", sep="\n")
EngBible <- EngBible[1:1000]

EngBible <- sapply(X = EngBible, FUN = clean)

## Shorten for testing purposes

data <- features(EngBible[[1]], language=1)

for (i in 1:length(EngBible)) {
  data[i ,] <- features(EngBible[[i]], language=1)
  print(c("English entry number", i))
}

beep()

###########################################################
################### German Bible #########################
###########################################################

GerBible <- scan("http://bitflow.dyndns.org/german/MartinLuther-1912/Martin_Luther_Uebersetzung_1912.txt",
                 what="character", sep="\n")

## Shorten for testing
GerBible <- GerBible[1:1000]

clean2 <- function(string){
  vec <- tolower(string)
  vec <- substring(vec, seq(1, nchar(vec)-1), seq(2, nchar(vec)))
  vec <- vec[-(1:3)]
  vec <- vec[vec %in% myletterpairs]
  return(vec)
}

GerBible <- sapply(X = GerBible, FUN = clean2)

for (i in 1:length(GerBible)) {
  data[length(EngBible)+i ,] <- features(GerBible[[i]], language=0)
  print(c("German entry number", i))
}

write.csv(data, file="bibleData4.csv")

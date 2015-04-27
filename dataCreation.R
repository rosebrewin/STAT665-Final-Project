###########################################################
############## Language Classification ####################
###########################################################

### Set up
myletters <- c(letters, " ")
Sys.setlocale('LC_ALL','C') 

clean <- function(string){
  string <- tolower(string)
  vec <- unlist(strsplit(string, split=""))
  vec <- vec[vec %in% myletters]
  return(vec)
}

freq <- function(vec) {
  ff <- sapply(myletters[-length(myletters)], FUN = function(x){mean(vec==x)})
  ff[is.na(ff)] <- 0
  return(ff)
}

features <- function(text, language) {
  mat <- matrix(nrow=length(myletters)-1, ncol=length(myletters)-1)
  for (i in 1:(length(myletters)-1)) {
    following <- text[which(text==myletters[i]) + 1]
    following <- following[!is.na(following)]
    mat[i, ] <- freq(following)
  }
  vec <- t(as.vector(mat))
  return(cbind(language, as.data.frame(vec)))
}

###########################################################
################### English Bible #########################
###########################################################

EngBible <- scan("http://www.gutenberg.org/cache/epub/10/pg10.txt", what="character", sep="\n")

EngBible <- sapply(X = EngBible, FUN = clean)

## Shorten for testing purposes
EngBible <- EngBible[1:1000]

data <- features(EngBible[[1]], language=1)


for (i in 1:length(EngBible)) {
  data[i ,] <- features(EngBible[[i]], language=1)
  print(c("English entry number", i))
}

then <- Sys.time()
now <- Sys.time()

beep()

###########################################################
################### German Bible #########################
###########################################################

GerBible <- scan("http://bitflow.dyndns.org/german/MartinLuther-1912/Martin_Luther_Uebersetzung_1912.txt",
                 what="character", sep="\n")

## Shorten for testing
GerBible <- GerBible[1:1000]

clean2 <- function(string){
  vec <- unlist(strsplit(string, split=""))
  vec <- vec[-(1:4)] # remove the 'Gen' bit
  vec <- vec[vec %in% myletters]
  return(vec)
}

GerBible <- sapply(X = GerBible, FUN = clean2)

for (i in 1:length(GerBible)) {
  data[length(EngBible)+i ,] <- features(GerBible[[i]], language=0)
  print(c("German entry number", i))
}

write.csv(data, file="bibleData3.csv")
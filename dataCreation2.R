Sys.setlocale('LC_ALL','C')

### Define letters used in data frame
myletters <- c(letters, " ")
myletterpairs <- matrix(nrow=length(myletters), ncol=length(myletters))
for (i in 1:length(myletters)) {
  for (j in 1:length(myletters)) {
    myletterpairs[j, i] <- paste(myletters[i], myletters[j], sep="")
  }
}
myletterpairs <- as.vector(myletterpairs)

### Define functions for data creation
clean <- function(string){
  vec <- tolower(string)
  vec <- substring(vec, seq(1, nchar(vec)-1), seq(2, nchar(vec)))
  vec <- vec[vec %in% myletterpairs]
  return(vec)
}

features <- function(vec, language) {
  vec <- sapply(myletterpairs, FUN = function(x){mean(vec==x)})
  vec[is.na(vec)] <- 0
  return(cbind(language, as.data.frame(t(vec))))
}

createData <- function(url, language) {
  bible <- read.table(url, sep='\n', stringsAsFactors=F, quote="")
  
  bible <- bible[-(1:25), ] # remove title etc
  bible <- bible[1:1000]
  bible <- sapply(X = bible, FUN = clean) 
  
  data <- features(bible[[1]], language=1) # initialise data frame
  
  for (i in 1:length(bible)) {
    data[i ,] <- features(bible[[i]], language=language)
  }
  
  write.csv(data, file=paste(language, "Bible.csv", sep=""), row.names=F)
}


### Create multiple bible data frames
createData("http://www.ccel.org/ccel/bible/webster.txt", "english")
createData("http://www.ccel.org/ccel/bible/delut.txt", "german")
createData("http://www.ccel.org/ccel/bible/frls.txt", "french")
createData("http://www.ccel.org/ccel/bible/esrv.txt", "spanish")
createData("http://www.ccel.org/ccel/bible/it.txt", "italian")
createData("http://www.ccel.org/ccel/bible/vul.txt", "latin")
createData("http://www.ccel.org/ccel/bible/nls.txt", "dutch")


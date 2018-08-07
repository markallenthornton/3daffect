

# load packages -----------------------------------------------------------
if(!require(stringr)) install.packages("stringr"); require(stringr)

# load dictionary ---------------------------------------------------------

load("3daffect.Rdata")
tokens[is.na(tokens)]<-"NA"
rownames(dict)<-tokens

# validate words ----------------------------------------------------------

# valence, arousal, and dominance ratings for nearly 14k English words
# downloaded from here: http://crr.ugent.be/archives/1003
norms <- read.csv("Ratings_Warriner_et_al.csv")
norms <- norms[as.character(norms$Word) %in% tokens,]

sdict <- dict[as.character(norms$Word),]

# valence
cor(sdict[,3],norms$V.Mean.Sum)

# arousal vs. social impact
cor(sdict[,2],norms$A.Mean.Sum)

# dominance vs. rationality
cor(sdict[,3],norms$D.Mean.Sum)



# validate sentences/passages ---------------------------------------------

# validation data sets from here: https://archive.ics.uci.edu/ml/datasets/Sentiment+Labelled+Sentences
# From Group to Individual Labels using Deep Features, Kotzias et. al,. KDD 2015 

# tokenize for fastText
ft_tokenize <- function(x){
  words <- str_split(x,boundary("word"))
  punct <- str_match_all(x,"[:punct:]")
  out <- list(words[[1]],punct[[1]])
  return(out)
}

# count unique tokens (accounting for double-counting of apostrophes)
tk_count<-function(x){
  tks <- ft_tokenize(x)
  mapos <- length(unlist(str_match_all(tks[[1]],"'")))
  tk.count <- table(c(tks[[1]],tks[[2]]))
  if ("'" %in% names(tk.count)){
    tk.count["'"] <- tk.count["'"]-mapos
  }
  return(tk.count)
}

# tokenize words
ft_tokenize <- function(x){
  words <- str_split(x,boundary("word"))
  return(words)
}

# count unique tokens
tk_count<-function(x){
  tks <- ft_tokenize(x)
  return(table(tks))
}

# compute sentiment score
sentiment <- function(x,tokens,scores){
  tk.count <- tk_count(x)
  tk.count <- tk.count[names(tk.count) %in% tokens]
  curtks <- names(tk.count)
  sel <- tokens %in% curtks
  curtokens <- tokens[sel]
  curscores <- scores[sel]
  vals <- mean(unlist(lapply(curtks, function(x) rep(curscores[curtokens==x],tk.count[x]))))
  return(mean(vals))
}

# amazon review sentences
amazon <- read.table("./sentiment labelled sentences/amazon_cells_labelled.txt",sep = "\t")
amazon.3d <- unlist(lapply(amazon$V1,function(x) sentiment(x,tokens,dict[,3])))
amazon.14k <- unlist(lapply(unlist(lapply(amazon$V1,tolower)),function(x) sentiment(x,as.character(norms$Word),norms$V.Mean.Sum)))
cor(amazon$V2,amazon.3d)
mean(is.na(amazon.14k))
cor(amazon$V2,amazon.14k,use="pairwise.complete.obs")
sum(table(amazon$V2,amazon.3d>median(amazon.3d))[c(1,4)])/length(amazon$V2)
sum(table(amazon$V2,amazon.14k>median(amazon.14k,na.rm=T))[c(1,4)])/length(amazon$V2)

# imdb review sentences
imdb <- read.table("./sentiment labelled sentences/imdb_labelled.txt",sep = "\t")
imdb.3d <- unlist(lapply(imdb$V1,function(x) sentiment(x,tokens,dict[,3])))
imdb.14k <- unlist(lapply(unlist(lapply(imdb$V1,tolower)),function(x) sentiment(x,as.character(norms$Word),norms$V.Mean.Sum)))
cor(imdb$V2,imdb.3d)
mean(is.na(imdb.14k))
cor(imdb$V2,imdb.14k,use="pairwise.complete.obs")
sum(table(imdb$V2,imdb.3d>median(imdb.3d))[c(1,4)])/length(imdb$V2)
sum(table(imdb$V2,imdb.14k>median(imdb.14k,na.rm=T))[c(1,4)])/length(imdb$V2)

# yelp review sentences
yelp <- read.table("./sentiment labelled sentences/yelp_labelled.txt",sep = "\t")
yelp.3d <- unlist(lapply(yelp$V1,function(x) sentiment(x,tokens,dict[,3])))
yelp.14k <- unlist(lapply(yelp$V1,function(x) sentiment(x,as.character(norms$Word),norms$V.Mean.Sum)))
cor(yelp$V2,yelp.3d)
mean(is.na(yelp.14k))
cor(yelp$V2,yelp.14k,use="pairwise.complete.obs")
sum(table(yelp$V2,yelp.3d>median(yelp.3d))[c(1,4)])/length(yelp$V2)
sum(table(yelp$V2,yelp.14k>median(yelp.14k,na.rm=T))[c(1,4)])/length(yelp$V2)





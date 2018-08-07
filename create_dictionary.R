

# load packages -----------------------------------------------------------

if(!require(e1071)) install.packages("e1071"); require(e1071)
library(data.table)

# define functions --------------------------------------------------------

rmse <- function(x,y){
  return(sqrt(mean((x-y)^2)))
}

# load (small) data -------------------------------------------------------

# pricipal components scores
pcs <- read.csv("pc166.csv")
states <- as.character(pcs$states)
pcs <- pcs[,2:4]
pcs[,1] <- -pcs[,1]
rownames(pcs)<-states
#pcs <- apply(pcs,2,rs01)

# vectors for states
svecs <- read.csv("state_vectors.csv",header=F)
rownames(svecs)<-svecs$V1
svecs <- svecs[,2:301]


# test method on state words ----------------------------------------------

# svm-regression version
set.seed(1)
cvinds <- sample(rep(1:5,34)[1:166])
cperf <- matrix(NA,5,3)
eperf <- matrix(NA,5,3)
for (i in 1:5){
  tsel <- cvinds == i
  pcs.train <- pcs[!tsel,]
  svecs.train <- svecs[!tsel,]
  pcs.test <- pcs[tsel,]
  svecs.test <- svecs[tsel,]
  for (j in 1:3){
    y <- pcs.train[,j]
    x <- as.matrix(svecs.train)
    fit <- svm(y~x,kernel="radial",cost = 4)
    x <- as.matrix(svecs.test)
    preds <- predict(fit,x)
    cperf[i,j]<-cor(preds,pcs.test[,j])
    eperf[i,j]<-rmse(preds,pcs.test[,j])
  }
}
colMeans(cperf)
colMeans(eperf)

# fit full model
svmlist <- list()
for (i in 1:3){
  y <- pcs[,i]
  x <- as.matrix(svecs)
  svmlist[[i]]<-svm(y~x,kernel="radial",cost = 4)
}
 

# cycle through vectors ---------------------------------------------------

#fast <- fread("./crawl-300d-2M.vec/crawl-300d-2M.vec",skip = 1)
#save(fast,file="fast.Rdata")

load("fast.Rdata")
fast <- as.data.frame(fast)



tokens <- as.character(fast$V1)
nvec <- dim(fast)[1]
dict <- matrix(NA,nvec,3)
start <- proc.time()
for (v in 1:nvec) {
  x <- matrix(as.numeric(fast[v,2:301]),1,300)
  for (i in 1:3){
    dict[v,i] <- predict(svmlist[[i]],x)
  }
  if ((v %% 1000)==0){
    print(v)
    print(proc.time()-start)
  }
}

save(tokens,dict,file = "3daffect.Rdata")

rownames(dict)<-tokens
colnames(dict)<-gsub("\\."," ",colnames(pcs))
write.csv(dict,"3daffect_dict.csv")

# 2 million tokens with weighted valence vs:
# 407 positive emotion and 501 negative emotion words in LIWC
# 1747 positive and 4086 negative emotion words in qdap








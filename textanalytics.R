##text analytics

install.packages(c("ggplot2","e1071","caret","quanteda","irlba","randomForest"))
spam.raw <- read.csv("spam.csv",stringsAsFactors = FALSE)
View(spam.raw)
spam.raw <- spam.raw[,1:2]
names(spam.raw) <- c("Label","Text") ##name the columns
View(spam.raw)

#check data to see if there is any missing value
length(which(!complete.cases(spam.raw)))
#o shows that the dataset is complete
##first step is always explore the data
##convert our class label into a factor

spam.raw$Label <- as.factor(spam.raw$Label)
##shows the percentage of ham and spam
prop.table(table(spam.raw$Label)) 
##86% legit and 13% spam
#let us take a look at the lenghts of the text next
spam.raw$TextLength <- nchar(spam.raw$Text, type = "chars", allowNA = FALSE, keepNA = NA)

summary(spam.raw$TextLength)

install.packages("caret")
library(caret) ##classification and regression training
help(package="caret")
set.seed(32984) ##setting random seed to produce reproducibiltiy
indexes <- createDataPartition(spam.raw$Label,times=1,p=0.7,list=FALSE)
train <- spam.raw[indexes,]
test <- spam.raw[-indexes,]
##verify proportions

prop.table(table(train$Label))
prop.table(table(test$Label))

##how do we represent text as a data frame - unstructure data to structured format
##make every into a column -tokenizations
##then we can construst a document frequency matrix 
train$Text[21]



install.packages("quanteda")
library(quanteda)
train.tokens <- tokens(train$Text,what="word",remove_numbers=TRUE,remove_punct=TRUE,remove_symbols=TRUE,remove_hyphens=TRUE)
train.tokens[[357]]
#lower case the tokens
train.tokens <- tokens_tolower(train.tokens)
train.tokens[[357]]
train.tokens <- tokens_select(train.tokens,stopwords(),selection = 'remove')
train.tokens[[357]]
train.tokens <- tokens_wordstem(train.tokens,language="english")
train.tokens[[357]]

##we now create a document frequency matrix

train.token.dfm <- dfm(train.tokens,tolower=FALSE,remove=stopwords())
train.tokens.matrix <- as.matrix(train.token.dfm)
View(train.tokens.matrix)


##per best practices, we will leverage cross validation(cv) as the basis of our modeling process
##setup the feature data frame with labels
train.tokens.df <- cbind(Label=train$Label,as.data.frame(train.token.dfm))
View(train.tokens.df)

#tokenization requires some additional preprocessing
names(train.tokens.df)[c(146)]

name(train.tokens.df) <- make.names(names(train.tokens.df)) ##shows error

set.seed(48743)
cv.folds <- createMultiFolds(train$Label,k=10,times=3)
cv.cntrl <- trainControl(method="repeatedcv",number=10,repeats=3,index=cv.folds)


install.packages("doSNOW")
library(doSNOW)
start.time <- Sys.time() ##let us time our program
##doesnt work
cl <- makeCluster(2,type="SOCK")
registerDoSNOW(cl) ##alerts carets that these instances of r studio are being used 
rpart.cv.1 <- train(Label ~ .,data=train.tokens.df,method="rpart",trControl=cv.cntrl,tuneLength=7)
stopCluster(cl)
total.time <- Sys.time()-start.time
rpart.cv.1
##




















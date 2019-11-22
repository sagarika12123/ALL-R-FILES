#READING THE CSV FILES INTO DATASET

ref1<-read.csv("RXFILE#1.csv",header = TRUE)
View(ref1)
ref2<-read.csv("RXFILE#2.csv",header=TRUE)
View(ref2)
yr11<-read.csv("YEAR1#1.csv",header=TRUE)
View(yr11)
yr12<-read.csv("YEAR1#2.csv",header=TRUE)
View(yr12)
yr21<-read.csv("year2#1.csv",header=TRUE)
View(yr21)
yr22<-read.csv("year2#2.csv",header=TRUE)
View(yr22)
yr31<-read.csv("year3#1.csv",header=TRUE)
View(yr31)
yr32<-read.csv("year3#2.csv",header=TRUE)
View(yr32)

##EXTRACTING IC CODES ONLY
library(data.table)
selectedRows11<-yr11[grep("I",yr11$Primary_Dx), ]
  View(selectedRows11)
selectedRows12<-yr12[grep("I",yr12$Primary_Dx), ]
selectedRows21<-yr21[grep("I",yr21$Primary_Dx), ]
selectedRows22<-yr22[grep("I",yr22$Primary_Dx), ]
selectedRows31<-yr31[grep("I",yr31$Primary_Dx), ]
selectedRows32<-yr32[grep("I",yr32$Primary_Dx), ]
########dont execute ################
##copy the claim id
id11<-data.frame(selectedRows11$Member_ID,selectedRows11$Allowed_Amt) 
View(id11)
#icd1<-data.frame(selectedRows11$Member_ID,selectedRows11$Primary_Dx)
#View(icd1)
id12<-data.frame(selectedRows12$Member_ID,selectedRows12$Allowed_Amt)
id21<-data.frame(selectedRows21$Member_ID,selectedRows21$Allowed_Amt)
id22<-data.frame(selectedRows22$Member_ID,selectedRows22$Allowed_Amt)
id31<-data.frame(selectedRows31$Member_ID,selectedRows31$Allowed_Amt)
id32<-data.frame(selectedRows32$Member_ID,selectedRows32$Allowed_Amt)
total<-merge.data.frame()

##sort the data
id11<-id11[order(id11$selectedRows11.Member_ID),]
id12<-id12[order(id12$selectedRows12.Member_ID),]
id21<-id21[order(id21$selectedRows21.Member_ID),]
id22<-id22[order(id22$selectedRows22.Member_ID),]
id31<-id31[order(id31$selectedRows31.Member_ID),]
id32<-id32[order(id32$selectedRows32.Member_ID),]

#########till here####################
library(dplyr) #used for data manipulation and data handling


#############CALCULATING COMORBIDITY INDEX###########################################################


##FIRST LET ME TRY FOR SAMPLE- RANDOMLY GENERATE ICD 10 CODES##
install.packages("comorbidity")
library(comorbidity)
#set.seed(1)
#simulate 50 icd-10 codes for 5 individuals
#x <- data.frame(
# id = sample(1:5, size = 50, replace = TRUE),
#  code = sample_diag(n = 50),
 # stringsAsFactors = FALSE
#)
#x <- x[order(x$id, x$code), ]
#x<-x[]
#print(head(x, n = 15), row.names = FALSE)
##its also possible to simulate from two diff versions of the ICD-10 coding system. the default is to simulate 
#icd 10 codes from 2011 version

#set.seed(1)
#x1 <- data.frame(
#  id = sample(1:3, size = 30, replace = TRUE),
 # code = sample_diag(n = 30),
#  stringsAsFactors = FALSE
#)
#set.seed(1)
#x2 <- data.frame(
#  id = sample(1:3, size = 30, replace = TRUE),
#  code = sample_diag(n = 30, version = "ICD10_2011"),
#  stringsAsFactors = FALSE
#)
# should return TRUE
#all.equal(x1, x2)

#Say we have 3 individuals with a total of 30 ICD-10 diagnostic codes:

#set.seed(1)
#x <- data.frame(
 # id = sample(1:3, size = 30, replace = TRUE),
 # code = sample_diag(n = 30),
 # stringsAsFactors = FALSE
#)
##we could compute the charlson score, index,and each comorbidity domain



#charlson <- comorbidity(x = x, id = "id", code = "code", score = "charlson", icd = "icd10", assign0 = FALSE)
#charlson
##we set assign 0 to false to not apply a heirarchy of comorbidity codes
#charlson.default <- comorbidity(x = x, id = "id", code = "code", score = "charlson", assign0 = FALSE)
#all.equal(charlson, charlson.default)
## [1] TRUE
icd11<-data.frame(selectedRows11$Member_ID,selectedRows11$Primary_Dx,selectedRows11$Dx_Code_1,selectedRows11$Dx_Code_2,selectedRows11$Dx_Code_3,selectedRows11$Dx_Code_4,selectedRows11$Dx_Code_4,selectedRows11$Dx_Code_5,selectedRows11$Dx_Code_6)
View(icd11)
print(head(icd11,n=50),row.names = FALSE)
charlson<-comorbidity(x=icd11,id="selectedRows11.Member_ID",code="selectedRows11.Primary_Dx,,score="charlson",assign0=FALSE)
charlson1<-comorbidity(x=icd11,id="selectedRows11.Member_ID",code="selectedRows11.Dx_Code_1",score="charlson",assign0=FALSE)
View(charlson1)
charlson
View(charlson)
selectedcharlsonRows111<-yr11[grep("1",charlson$score), ] ##yr11##charlson
selectedcharlsonRows112 <- y12[grep("0",charlson$score),] ##yr12##charlson
View(selectedcharlsonRows111)

library(ggplot2)


x1 <- selectedcharlsonRows111$Allowed_Amt ##which have high comorbid index 
x2<-selectedcharlsonRows112$Allowed_Amt ##which have low comorbid index
View(x1)
View(x2)
x1<-as.data.frame(x1)
x2<-as.data.frame(x2)
class(x1)


x1<-data.frame(selectedcharlsonRows111$Member_ID,selectedcharlsonRows111$Allowed_Amt,selectedcharlsonRows111$Claim_Number)
x2<-data.frame(selectedcharlsonRows112$Member_ID,selectedcharlsonRows112$Allowed_Amt,selectedcharlsonRows112$Claim_Number)
library(yarrr)

pirateplot(formula=selectedcharlsonRows111$Allowed_Amt~selectedcharlsonRows111$Member_ID+selectedcharlsonRows111$Claim_Number,
           data = x1, #Data frame
           xlab = NULL, ylab = "distinct member id clubbed under claim id", #Axis labels
           main = "Cost fluctuation based on claim number, member_id", #Plot title
           pal = "google", #Color scheme
           point.o = 10, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 1, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7)
##pirateplotting with high comorbidity index

yarrr::pirateplot(formula=selectedcharlsonRows111$Allowed_Amt~selectedcharlsonRows111$Claim_Number,
                  data=x1,
                  main="pirateplotting with high comorbidity index",
                  xlab="claim utilization",
                  ylab="cost utilization",
                  point.o = .8,   # Turn up points
                  bar.f.o = .8, # Turn up bars
                  bean.f.o = .8, # Light bean filling
                  bean.b.o = .8, # Light bean border
                  avg.line.o = 1, # Turn off average line
                  point.col = "black")
##pirateplotting with less comorbidity index
yarrr::pirateplot(formula=selectedcharlsonRows112$Allowed_Amt~selectedcharlsonRows112$Claim_Number,
                  data=x2,
                  main="pirateplotting with less comorbidity index",
                  xlab="claim utilization",
                  ylab="cost utilization",
                  point.o = .8,   # Turn up points
                  bar.f.o = .8, # Turn up bars
                  bean.f.o = .8, # Light bean filling
                  bean.b.o = .8, # Light bean border
                  avg.line.o = 1, # Turn off average line
                  point.col = "black")


##creating comorbidity network:






















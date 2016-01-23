library(dplyr)
library(ggplot2)

#extracting L1 = K and L2 = E in urban
urb_school_L2_data <- filter(g6, URBAN_RURAL == "U", L1_CODE == "01K",L2_CODE == "31E")
str(urb_school_L2_data)
urb_l2_mean <- mean(urb_school_L2_data$L2_MARKS)

table(urb_school_L2_data$L2_MARKS)

#extracting L1 = K and L2 = E in rural
rur_school_L2_data <- filter(g6, URBAN_RURAL == "R", L1_CODE == "01K",L2_CODE == "31E")
str(rur_school_L2_data)
rur_l2_mean <- mean(rur_school_L2_data$L2_MARKS)

#drawing histogram for both data
library(ggplot2)
#ggplot(urb_school_L2_data, aes(x=urb_school_L2_data$L2_MARKS)) + geom_histogram(binwidth=2, color = "black", fill = "#FFFFE0")
#ggplot(rur_school_L2_data, aes(x=rur_school_L2_data$L2_MARKS)) + geom_histogram(binwidth=2, color = "black", fill = "#FFFFE0")

#calculating mean marks in L2 = E
urb_l2_mean <- mean(urb_school_L2_data$L2_MARKS)
rur_l2_mean <- mean(rur_school_L2_data$L2_MARKS)

#Percentage of students failed in L2 = E in urban
#urb_l2_fail <- filter( urb_school_L2_data,L2_RESULT == "F")
urb_l2_fail_perc <- (sum( urb_school_L2_data$L2_RESULT == "F") / sum(urb_school_L2_data$L2_RESULT=='F' | urb_school_L2_data$L2_RESULT=='P')) * 100
urb_l2_fail_perc #12.46%

#Percentage of students failed in L2 = E in rural
#rur_l2_fail <- filter(rur_school_L2_data, L2_RESULT == "F")
rur_l2_fail_perc <-  (sum( rur_school_L2_data$L2_RESULT == "F") / sum(rur_school_L2_data$L2_RESULT=='F' | rur_school_L2_data$L2_RESULT=='P')) * 100
rur_l2_fail_perc #11.60%

gal_fail_data <- data.frame(Area = factor(c("UrbanFail","RuralFail"), levels=c("UrbanFail","RuralFail")), Percentage = c(urb_l2_fail_perc, rur_l2_fail_perc))

ggplot(data=gal_fail_data, aes(x=Area, y=Percentage, fill=Area)) + geom_bar(stat="identity")
#extracting L1 = E and L2 = K in urban
urb_l1Eng_data <- filter(g6, URBAN_RURAL == "U", L1_CODE == "14E",L2_CODE == "33K")
urb_l1Eng_mean <- mean(urb_l1Eng_data$L1_MARKS)
urb_l1Eng_mean

#extracting L1 = E and L2 = K in rural
rur_l1Eng_data <- filter(g6, URBAN_RURAL == "R", L1_CODE == "14E",L2_CODE == "33K")
rur_l1Eng_mean <- mean(urb_l1Eng_data$L1_MARKS)
rur_l1Eng_mean

#Percentage of students failed in L1 = E in urban
#urb_l1Eng_fail <- filter(urb_l1Eng_data, L2_RESULT == "F")
urb_l1Eng_fail_perc <- (sum( urb_l1Eng_data$L2_RESULT == "F") / sum(urb_l1Eng_data$L2_RESULT=='F' | urb_l1Eng_data$L2_RESULT=='P')) * 100
urb_l1Eng_fail_perc

#Percentage of students failed in L1 = E in rural
#rur_l1Eng_fail <- filter(rur_l1Eng_data, L2_RESULT == "F")
rur_l1Eng_fail_perc <-  (sum( rur_l1Eng_data$L2_RESULT == "F") / sum(rur_l1Eng_data$L2_RESULT=='F' | rur_l1Eng_data$L2_RESULT=='P')) * 100
rur_l1Eng_fail_perc

gal_fail_data <- data.frame(Area = factor(c("UrbanFail","RuralFail"), levels=c("UrbanFail","RuralFail")), Percentage = c(urb_l1Eng_fail_perc, rur_l1Eng_fail_perc))

ggplot(data=gal_fail_data, aes(x=Area, y=Percentage, fill=Area)) + geom_bar(stat="identity")

###########################################################################################
#Measuring performance of students in different castes
caste1_data <- filter(g6, NRC_CASTE_CODE == 1)#SC
count(caste1_data)
mean(caste1_data$TOTAL_MARKS)
ggplot(caste1_data, aes(x=caste1_data$TOTAL_MARKS)) + geom_histogram(binwidth=5, color = "black", fill = "#FFFFE0")


caste2_data <- filter(g6, NRC_CASTE_CODE == 2)#ST
count(caste2_data)
mean(caste2_data$TOTAL_MARKS)
ggplot(caste2_data, aes(x=caste2_data$TOTAL_MARKS)) + geom_histogram(binwidth=5, color = "black", fill = "#FFFFE0")


caste3_data <- filter(g6, NRC_CASTE_CODE == 3)#Cat1
count(caste3_data)
mean(caste3_data$TOTAL_MARKS)
ggplot(caste3_data, aes(x=caste3_data$TOTAL_MARKS)) + geom_histogram(binwidth=5, color = "black", fill = "#FFFFE0")


caste4_data <- filter(g6, NRC_CASTE_CODE == 4)#Gen
count(caste4_data)
mean(caste4_data$TOTAL_MARKS)
ggplot(caste4_data, aes(x=caste4_data$TOTAL_MARKS)) + geom_histogram(binwidth=5, color = "black", fill = "#FFFFE0")

#Failed student percentage in all categories
#SC
#caste1_fail_data <- filter(caste1_data, NRC_RESULT == 'F')
caste1_fail_perc <- (sum(g6$NRC_RESULT=='F' & g6$NRC_CASTE_CODE==1)/sum(g6$NRC_CASTE_CODE==1))*100
caste1_fail_perc

#ST
#caste2_fail_data <- filter(caste2_data, NRC_RESULT == 'F')
caste2_fail_perc <- (sum(g6$NRC_RESULT=='F' & g6$NRC_CASTE_CODE==2)/sum(g6$NRC_CASTE_CODE==2))*100
caste2_fail_perc

#Cat1
#caste3_fail_data <- filter(caste3_data, NRC_RESULT == 'F')
caste3_fail_perc <- (sum(g6$NRC_RESULT=='F' & g6$NRC_CASTE_CODE==3)/sum(g6$NRC_CASTE_CODE==3))*100
caste3_fail_perc

#Gen
#caste4_fail_data <- filter(caste4_data, NRC_RESULT == 'F')
caste4_fail_perc <- (sum(g6$NRC_RESULT=='F' & g6$NRC_CASTE_CODE==4)/sum(g6$NRC_CASTE_CODE==4))*100
caste4_fail_perc


caste_fail_data <- data.frame(Area = factor(c("SC","ST","Category 1","General"), levels=c("SC","ST","Category 1","General")), Percentage = c(caste1_fail_perc, caste2_fail_perc,caste3_fail_perc,caste4_fail_perc))

ggplot(data=caste_fail_data, aes(x=Area, y=Percentage, fill=Area)) + geom_bar(stat="identity")

#Performing Association Rule Mining on Caste Data
library(arules)
arule_data <- g6[,c(6,12,13,30)]
arule_data$NRC_CASTE_CODE <- as.factor(arule_data$NRC_CASTE_CODE)
tbl_df(arule_data)
rules <- apriori(arule_data)
inspect(rules)
rules <- apriori(arule_data, parameter = list(minlen = 2, supp = 0.005, conf = 0.6), appearance = list(rhs=c("NRC_RESULT=F","NRC_RESULT=P"),default = "lhs"), control = list(verbose = T))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#pruning redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
#plotting the arules
library(arulesViz)
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))

######################################################################################
#Performing Discretization + Classification
library(rattle)
library(rpart.plot)
library(RColorBrewer)
g6$L1_DESC<-cut(g6$L1_MARKS,c(0,30,45,60,80,100),labels=c('F','P','2','1','D'))
g6$L2_DESC<-cut(g6$L2_MARKS,c(0,30,45,60,80,100),labels=c('F','P','2','1','D'))
g6$L3_DESC<-cut(g6$L3_MARKS,c(0,30,45,60,80,100),labels=c('F','P','2','1','D'))
g6$S1_DESC<-cut(g6$S1_MARKS,c(0,30,45,60,80,100),labels=c('F','P','2','1','D'))
g6$S2_DESC<-cut(g6$S2_MARKS,c(0,30,45,60,80,100),labels=c('F','P','2','1','D'))
g6$S3_DESC<-cut(g6$S3_MARKS,c(0,30,45,60,80,100),labels=c('F','P','2','1','D'))
ind<-sample(2,nrow(g6),replace = TRUE,prob = c(0.7,0.3))
train<-g6[ind==1,]
test<-g6[ind==2,]
myf<-NRC_CLASS~L1_DESC+L2_DESC+L3_DESC+S1_DESC+S2_DESC+S3_DESC
tree1<-rpart(myf,data = train,control = rpart.control(minsplit = 10))
plot(tree1)
text(tree1)
print(tree1)
fancyRpartPlot(tree1)

#predicting the NRC_CLASS
Prediction <- predict(tree1, test, type = "class")

submit <- data.frame(original_value= test$NRC_CLASS,predicted_value=Prediction)

table(submit$original_value==submit$predicted_value)

xtab <- table(submit$original_value,submit$predicted_value)

library(caret)
confusionMatrix(xtab) #all necessary parameters

######################################################################################
#Calculating No of girls
urb_gal <- filter(g6, URBAN_RURAL == "U", NRC_GENDER_CODE == "G")
rur_gal <- filter(g6, URBAN_RURAL == "R", NRC_GENDER_CODE == "G")

count(urb_gal)
count(rur_gal)
library(ggplot2)

#plotting histograms of total marks distribution
ggplot(urb_gal, aes(x=urb_gal$TOTAL_MARKS)) + geom_histogram(binwidth=5, color = "black", fill = "#FFFFE0")
mean(urb_gal$TOTAL_MARKS)
ggplot(rur_gal, aes(x=rur_gal$TOTAL_MARKS)) + geom_histogram(binwidth=5, color = "black", fill = "#FFFFE0")
mean(rur_gal$TOTAL_MARKS)

#No of girls failed in rural and urban areas
urb_gal_fail <- filter(urb_gal, NRC_RESULT == "F")
count(urb_gal_fail)
rur_gal_fail <- filter(rur_gal, NRC_RESULT == "F")
count(rur_gal_fail)

#Percentage of girls failed in Urban and Rural areas
#urb_gal_fail_perc <- (sum(urb_gal_fail)/sum(urb_gal))*100
#urb_gal_fail_perc #19.45%
#rur_gal_fail_perc <- (sum(rur_gal_fail)/sum(rur_gal))*100
#rur_gal_fail_perc #17.48

urb_gal_fail_perc <- (sum(g6$URBAN_RURAL == 'U' & g6$NRC_RESULT == 'F' & g6$NRC_GENDER_CODE == "G")/sum(g6$URBAN_RURAL == 'U' & g6$NRC_GENDER_CODE == "G"))*100
urb_gal_fail_perc
rur_gal_fail_perc <- (sum(g6$URBAN_RURAL == 'R' & g6$NRC_RESULT == 'F' & g6$NRC_GENDER_CODE == "G")/sum(g6$URBAN_RURAL == 'R' & g6$NRC_GENDER_CODE == "G"))*100


#Bar Chart of girls failing in Urban vs Rural
gal_fail_data <- data.frame(Area = factor(c("Urban_Girls","Rural_Girls"), levels=c("Urban_Girls","Rural_Girls")), Percentage = c(urb_gal_fail_perc, rur_gal_fail_perc))

ggplot(data=gal_fail_data, aes(x=Area, y=Percentage, fill=Area)) + geom_bar(stat="identity")
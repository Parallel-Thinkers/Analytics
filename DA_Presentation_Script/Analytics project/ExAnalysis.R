#summarising the Data
summary(g6)

#comparing the divisions of students
w = table(g6$NRC_CLASS)

t = as.data.frame(w)

perc <- round(t$Freq/sum(t$Freq)*100)
lbls <- paste(t$Var1, perc) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

library(plotrix)
pie3D(t$Freq, labels = lbls, explode = 0.1, main = "Pie chart of Divisions")

#comparing Urban & Rural areas in context of PASS result
urb_pass_per <- (sum(g6$URBAN_RURAL == 'U' & g6$NRC_RESULT == 'P')/sum(g6$URBAN_RURAL == 'U'))*100
rur_pass_per <- (sum(g6$URBAN_RURAL == 'R' & g6$NRC_RESULT == 'P')/sum(g6$URBAN_RURAL == 'R'))*100

dat_pass <- data.frame(Area = factor(c("Urban","Rural"), levels=c("Urban","Rural")), Percentage = c(urb_pass_per, rur_pass_per))

ggplot(data=dat_pass, aes(x=Area, y=Percentage, fill=Area)) + geom_bar(stat="identity")

#comparing Urban & Rural areas in context of FAIL result
urb_fail_per <- (sum(g6$URBAN_RURAL == 'U' & g6$NRC_RESULT == 'F')/sum(g6$URBAN_RURAL == 'U'))*100
rur_fail_per <- (sum(g6$URBAN_RURAL == 'R' & g6$NRC_RESULT == 'F')/sum(g6$URBAN_RURAL == 'R'))*100

dat_fail <- data.frame(Area = factor(c("Urban","Rural"), levels=c("Urban","Rural")), Percentage = c(urb_fail_per, rur_fail_per))

ggplot(data=dat_fail, aes(x=Area, y=Percentage, fill=Area)) + geom_bar(stat="identity")

#analysing and comparing marks obtained by students in all the 6 exams via box plots
color <- c("blue","green","red","yellow","purple","turquoise")

exam_name <- c("L1_Marks","L2_Marks","L3_Marks","S1_Marks","S2_Marks","S3_Marks")

boxplot(g6$L1_MARKS, g6$L2_MARKS, g6$L3_MARKS, g6$S1_MARKS,g6$S2_MARKS, g6$S3_MARKS, data = g6, col = color, names = exam_name, main = "Box Plot Comparision of Exams")

#analysing variation of percentage of BOYS via histogram
library(dplyr)
library(ggplot2)
boy_data <- filter(g6, NRC_GENDER_CODE == 'B')

Percentage <- (boy_data$TOTAL_MARKS/650)*100
ggplot(boy_data, aes(x=Percentage)) + geom_histogram(binwidth=2, color = "black", fill = "#FFFFE0")

#plotting density graph of the same data
ggplot(boy_data, aes(x=Percentage)) + geom_histogram(aes(y=..density..), binwidth=2, colour="black", fill="white") +geom_density(alpha=.3, fill="#FF6666")

#analysing variation of percentage of GIRLS via histogram
girl_data <- filter(g6, NRC_GENDER_CODE == 'G')

Percentage <- (girl_data$TOTAL_MARKS/650)*100
ggplot(girl_data, aes(x=Percentage)) + geom_histogram(binwidth=2, color = "black", fill = "#FFFFE0")

#plotting density graph of the same data
ggplot(girl_data, aes(x=Percentage)) + geom_histogram(aes(y=..density..), binwidth=2, colour="black", fill="white") +geom_density(alpha=.3, fill="#FF6666")

#histogram showing
fail <- filter(g6, NRC_RESULT == 'F')
fail_per <- (fail$TOTAL_MARKS/650)*100

ggplot(fail, aes(x=fail_per)) + geom_histogram(binwidth=2, color = "black", fill = "#FFFFE0")

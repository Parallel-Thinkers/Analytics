#read the data into R workspace
g6 <- read.csv("F:/M. Tech/3rd Sem/Data Analytics/R dir/Group6Data.csv")

#View the data as GUI
View(g6)

#remove '*' from each of the numeric column
g6$L1_MARKS <- as.numeric(gsub('[^a-zA-Z0-9.]', '', g6$L1_MARKS))
g6$L2_MARKS <- as.numeric(gsub('[^a-zA-Z0-9.]', '', g6$L2_MARKS))
g6$L3_MARKS <- as.numeric(gsub('[^a-zA-Z0-9.]', '', g6$L3_MARKS))
g6$S1_MARKS <- as.numeric(gsub('[^a-zA-Z0-9.]', '', g6$S1_MARKS))
g6$S2_MARKS <- as.numeric(gsub('[^a-zA-Z0-9.]', '', g6$S2_MARKS))
g6$S3_MARKS <- as.numeric(gsub('[^a-zA-Z0-9.]', '', g6$S3_MARKS))

#replace NA with mean
g6$L1_MARKS[is.na(g6$L1_MARKS)] <- mean(g6$L1_MARKS, na.rm = TRUE)
g6$L2_MARKS[is.na(g6$L2_MARKS)] <- mean(g6$L2_MARKS, na.rm = TRUE)
g6$L3_MARKS[is.na(g6$L3_MARKS)] <- mean(g6$L3_MARKS, na.rm = TRUE)
g6$S1_MARKS[is.na(g6$S1_MARKS)] <- mean(g6$S1_MARKS, na.rm = TRUE)
g6$S2_MARKS[is.na(g6$S2_MARKS)] <- mean(g6$S2_MARKS, na.rm = TRUE)
g6$S3_MARKS[is.na(g6$S3_MARKS)] <- mean(g6$S3_MARKS, na.rm = TRUE)

#replacing date with default value
g6$DOB <- as.character(g6$DOB)
g6$DOB[is.na(g6$DOB)] <-"0/0/0000 0:00" 

#replacing blanks with mean value
blanks <- is.na(g6$L1_RESULT)
g6$L1_MARKS[blanks] <- mean(g6$L1_MARKS, na.rm = TRUE)
g6$L1_RESULT[blanks] <- "P"

blanks <- is.na(g6$L2_RESULT)
g6$L2_MARKS[blanks] <- mean(g6$L2_MARKS, na.rm = TRUE)
g6$L2_RESULT[blanks] <- "P"

blanks <- is.na(g6$L3_RESULT)
g6$L3_MARKS[blanks] <- mean(g6$L3_MARKS, na.rm = TRUE)
g6$L3_RESULT[blanks] <- "P"

#remove outliers
g6$L1_MARKS[g6$L1_MARKS == 888] <- 0
g6$L2_MARKS[g6$L2_MARKS == 888] <- 0
g6$L3_MARKS[g6$L3_MARKS == 888] <- 0
g6$S1_MARKS[g6$S1_MARKS == 888] <- 0
g6$S2_MARKS[g6$S2_MARKS == 888] <- 0
g6$S3_MARKS[g6$S3_MARKS == 888] <- 0

#set total marks after changing the subject marks
g6$TOTAL_MARKS = g6$L1_MARKS + g6$L2_MARKS + g6$L3_MARKS + g6$S1_MARKS + g6$S2_MARKS + g6$S3_MARKS

#dividing students into new classes
g6$NRC_CLASS <- as.character(g6$NRC_CLASS)

g6$NRC_CLASS[(g6$TOTAL_MARKS/650)*100 >= 80 & g6$NRC_RESULT == "P"] <- "D"

g6$NRC_CLASS[(g6$TOTAL_MARKS/650)*100 < 80 & (g6$TOTAL_MARKS/650)*100 >=60 & g6$NRC_RESULT == "P"] <- "1"

g6$NRC_CLASS[(g6$TOTAL_MARKS/650)*100 < 60 & (g6$TOTAL_MARKS/650)*100 >=50 & g6$NRC_RESULT == "P"] <- "2"

g6$NRC_CLASS[(g6$TOTAL_MARKS/650)*100 < 50 & (g6$TOTAL_MARKS/650)*100 >=35 & g6$NRC_RESULT == "P"] <- "PASS"

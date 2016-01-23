library(dplyr)

urb_data <- filter(g6, URBAN_RURAL == 'U')
rur_data <- filter(g6, URBAN_RURAL == 'R')

urb_lang <- urb_data[c(17,19,21)]
urb_lang <- scale(urb_lang)

rur_lang <- rur_data[c(17,19,21)]

#cl<-kmeans(urb_lang[1:500,],4,iter.max=15)
cl=kmeans(urb_lang[1:500,],4)
plot(urb_data[c(17,19)],col=cl$cluster)
#clusplot(urb_lang[1:500,], cl$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
#         main = paste('Major Market Clusters over'), sub='')
aggregate(urb_lang[1:500,c(1,2)],by=list(cl$cluster),FUN=mean)
# append cluster assignment
c1 <- data.frame(urb_lang[1:500,c(1,2)], cl$cluster) 


urb_l1_code <- urb_data[c(34,35,36)]
d <- dist(urb_l1_code[1:100,c(1,2,3)])
fit <- hclust(d,method="complete")
plot(fit)
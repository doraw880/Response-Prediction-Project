myData <- read.csv(file.choose(), header = TRUE, sep = ",")

# Plot the target variable against all the numeric features
# We seperated all the numeric features into three group, and did the scatter plot

# Create a sample function
sample_data<-function(data, n){
  # Seperate the data between isResponse=0 and isResponse=1
  data_0<-data[which(data$isResponse==0),]
  data_1<-data[which(data$isResponse==1),]
  
  # Sample from the data of isResponse equaling zeros and ones
  index_0<-sample(1:nrow(data_0), n, replace=TRUE)
  index_1<-sample(1:nrow(data_1), n, replace=TRUE)
  
  # Combine ones and sampling zeros together
  sampleData<-rbind(data_0[index_0,], data_1[index_1, ])
  row.names(sampleData) <- NULL
  sampleData <- droplevels(sampleData)
}

# Plot all the credit score data
data1<-cbind(isResponse=myData$isResponse, myData[,grep("*credit_score*",names(myData))])
plot(data1, col=ifelse(data1$isResponse==1, "red", "blue"))
# Comment: score_fraud and score_market seem more significant to isResponse

# Plot all the match data
data2<-cbind(isResponse=myData$isResponse, myData[,grep("*match*",names(myData))])
data2<-data2[,sapply(data2, is.numeric)]
plot(data2, col=ifelse(data2$isResponse==1, "red", "blue"))
# Comment: first and address are significant to isResponse

# Plot all the demo data
data3<-cbind(isResponse=myData$isResponse, myData[,grep("*demo*",names(myData))])
data3<-data3[,sapply(data3, is.numeric)]
plot(data3, col=ifelse(data3$isResponse==1, "red", "blue"))
# Comment: income, homevalue, age_years and education_years are significant

# Plot all the credit_score data, and save it as png
data_all_n<-cbind(isResponse=myData$isResponse, myData[,56:ncol(myData)])
sample_all_n<-sample_data(data_all_n, 75)
png("data_sample_numeric_3.png", units="px", width=1600, height=1600, res=300)
plot(sample_all_n[2:30], col=ifelse(sample_all_n$isResponse==1, "red", "blue"))
dev.off()

# Plot the target variable against all the categorical features
# We used ggplot to plot the bar chart of the count of both isResponses equaling ones and zeros for each category each feature

# Selected all the categorical data
cateData<-cbind(isResponse=myData$isResponse, myData[,sapply(myData, is.factor)])

# Plot the whole categorical data
for (i in 2:ncol(cateData)){
  name<-names(cateData)[i]
  b<-ggplot(cateData, aes(x = factor(cateData[name][[1]]), fill = factor(isResponse), 
                          y = (..count..))) +
    geom_bar() +
    xlab(name) + ylab("count")
  theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
  b  
  ggsave(paste(name,'.png'))
}
# Because the whole data is highly unbalanced, 
# the pecentage of ones of isResponse is so low that we cannot figure out which one is significant. 
# Thus we sampled the zeros, and combined them with zeros.


# Create a sample sub data set which have 196 rows of isResponse=0, and all 93 rows of isResponse=1
sampleData<-sample_data(cateData, 75)
sampleData[sampleData==""] <- NA

# Plot bar charts and save them as png files
for (i in 2:ncol(sampleData)){
  name<-names(cateData)[i]
  b<-ggplot(sampleData, aes(x = factor(sampleData[name][[1]]), fill = factor(isResponse), 
                            y = (..count..))) +
    geom_bar() +
    xlab(name) + ylab("count")
  theme_update(axis.text.x = element_text(angle = 90, hjust = 1))
  b  
  ggsave(paste(name,'.png'))
}
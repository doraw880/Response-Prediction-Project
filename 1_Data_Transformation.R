# Read the .tsv file
myData <- read.delim(file.choose(), header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE)

# Pick all the columns which have data in atleast one of the rows and exclude data which only have NAs
newData <- myData[,sapply(myData, function (x) !all(is.na(x)))]

# Column bind the important columns of the dataset with the columns which start with eb_
newData <- cbind(myData[,1:6],newData)

# create a list of columns to be excluded from the normalizing
excluded_colnames <- c(names(newData[,grep("*zip*",names(newData))]),
                         c("index", "isResponse"))

# Check for all numeric columns
numeric_data <- newData[,sapply(newData, is.numeric)] 

# Preprocessing step to remove the excluded columns
normalized_data <- numeric_data[!(names(numeric_data) %in% excluded_colnames)]

# Function to normalize
# Divide each value of the column by the maximum value
normalize_fun <- function(x){
  x/max(x, na.rm=TRUE)  
}

# Apply the normalize function
normalized_data<-apply(normalized_data, 2, normalize_fun)

# Convert to a data frame.
result_data<-data.frame(normalized_data)

# Rebind the columns with the normalized data
result_data<-cbind(newData[!(names(newData) %in% names(result_data))],result_data)

# Add validated data of city, state, long, lat
library(zipcode)
data(zipcode)

newData <- merge(result_data, zipcode, by="zip")
names(newData)[which(names(newData)=='city.y')]="newCity"
names(newData)[which(names(newData)=='state.y')]="newState"
names(newData)[which(names(newData)=='city.x')]="city"
names(newData)[which(names(newData)=='state.x')]="state"

# Write the result to csv file
result_data<-newData
write.csv(result_data, file="newResult2.csv", row.names=F)

# Check for columns with very high NA counts 
count_print<-function(x){
  percentage<-NULL
  if (sum(is.na(x)) > 0) { percentage<-100*sum(is.na(x))/length(x) }
  
  if (length(which(x == "")) > 0) { percentage<-100*length(which(x == ""))/length(x) }
  percentage
}

apply(newData, 2, count_print)

# Check the excluded columns
excluded_columns<-names(result_data)[grep("^status|index|_zip?|city|state|longitude|latitude|newCity", names(result_data))]

# Create the analysis data
analysisData<-newData[!(names(newData) %in% excluded_columns)]

# Check each column and deal with NAs
# For numeric columns, replace NAs with mean
# For categorical columns, replace NAs with a new categorical -1
my_levels<-apply(analysisData, 2, function(x) nlevels(as.factor(x)))

for(i in 1:length(my_levels)){
  colname<-names(my_levels[i])
  if(my_levels[i]<=10|(class(analysisData[,i]) %in% c("factor", "character"))){
    analysisData[,i]<-as.factor(analysisData[,i])
    levels(analysisData[,i])<-c(levels(analysisData[,i]), "-1")
    analysisData[,i][is.na(analysisData[,i])|(analysisData[,i]=="")]<-"-1"
    analysisData[,i]<-droplevels(analysisData[,i])
  } else {
    mean<-mean(analysisData[,i], na.rm=T)
    analysisData[,i][is.na(analysisData[,i])|(analysisData[,i]=="")]<-mean
  }
}

# Write the analysis data to csv file
write.csv(analysisData, file="analysis_data.csv", row.names=F)

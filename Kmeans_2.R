install.packages("NbClust")
install.packages("factoextra")
install.packages("psych")
installed.packages("ggplot2")
install.packages("dplyr")

###IMPORTING LIBRARIES
#Below code will import all the required libraries.

library(NbClust)
library(dplyr)
library(psych)
library(factoextra) 
library(ggplot2)


###DATA ACCQUISTION
#Below code will import the data present in CSV format into R and store it in a Data Frame.

Holiday_Data <- read.csv("Data/buddymove_holidayiq.csv")

#View the data

str(Holiday_Data,5)




###DATA CLEANING
#Omit rows with Null values if any from the data

Holiday_Data <- na.omit(Holiday_Data)


#### Creating subset of required columns
#Create a subset without User Id column as for K means only numerical columns are chosen 

Holiday_Data_num <- Holiday_Data[,-1]


#### Scaling data
#Visualizing data distribution before scaling using baxplot

boxplot(Holiday_Data_num, outline= FALSE)

#Since clustering algorithms including kmeans use distance-based measurements to determine the similarity between data points, it’s recommended to #standardize the data to have a mean of zero and a standard deviation of one since almost always the features in any dataset would have different units of #measurements.

Holiday_Data_num <- as.data.frame(lapply(Holiday_Data_num, scale))

#Visualizing data distribution after scaling

boxplot(Holiday_Data_num, outline= FALSE)


#verify mean = 0 and standard deviation = 1

describe(Holiday_Data_num, fast=T)

#assigning background color and border line for graphs

fill <- "gold1"
line <- "goldenrod2"


### Descriptive Analysis
#Duplicate data just for exploatory purpose, we will be creating some calculated columns and don't want to alter main dataset.

Holiday_Data_Explore <- Holiday_Data


#### Top 5 active users based on total reviews given
#Identify Total reviews of each user

Holiday_Data_Explore$Total <- rowSums(Holiday_Data_Explore[,c("Sports","Religious","Nature", "Theatre", "Shopping","Picnic")]) 

#Sort data based on Total Number of reviews

Holiday_Data_Explore <- Holiday_Data_Explore %>% 
  arrange(Total)

#Visualize Top 5 active users using bar graph

ggplot(data=tail(Holiday_Data_Explore,5), aes(x=User.Id, y=Total)) +
  geom_bar(stat = "identity",fill = fill, colour = line) +
  geom_text(aes(label=Total), vjust=1.6, color="black", size=3.5)+
  theme_minimal() + 
  ggtitle("Top 5 Active Users") + 
  xlab("User Id") +
  ylab("Total NUmber of Reviews")


#### Top 5 shopaholics users
#Sort data based on Total Number of shopping reviews

Holiday_Data_Explore <- Holiday_Data_Explore %>% 
  arrange(Shopping)

#Bar graph to repreent users who shop most

ggplot(data=tail(Holiday_Data_Explore,5), aes(x=User.Id, y=Total)) +
  geom_bar(stat = "identity",fill = fill, colour = line) +
  geom_text(aes(label=Total), vjust=1.6, color="black", size=3.5)+
  theme_minimal() + 
  ggtitle("Top 5 Shopaholics") + 
  xlab("User Id") +
  ylab("Total NUmber of Reviews")



#Graph below will perform three analysis
#1. Distribution of data for each feature or column shown using a histogram.
#2. Correlation coefficients for each combination of feature
#3. Scatterplot with fit line to visualize correlation

pairs.panels(Holiday_Data_Explore[,c("Sports","Religious","Nature", "Theatre", "Shopping","Picnic")])

 #Key Observations 
#1. Number of shopping reviews has a strong positive relation with number of relegious destination reviews.
#2. Number of reviews by user for Shopping and Nature destinations shows weak negative corelation 
#3. Number of reviews by user for Relegious and Nature destinations shows weak negative corelation 
#4. Number of reviews by user for Nature and Theatre destinations shows weak positive corelation.


#### Most Reviewed Destinations
#First get column wise sums for each destination type and then sort it in decreaseing order

mostVoted <- order(colSums(Holiday_Data_Explore[,c("Sports","Religious","Nature", "Theatre", "Shopping","Picnic")]),decreasing=TRUE)

#Get the name of the column from main data frame
#Note <- +1 is done because in mostVoted we ignored first column i.e. User.Id

colnames(Holiday_Data_Explore)[mostVoted[1]+1]


#### K Means Clustering
##### Identify best value of K
#create empty lists

k <- list()
betweenss <- list()
totalss <- list()
ratio <- list()

#Elbow method to identify K value.
#For loop will calculate ratio of between cluster sum of squares and Total within clusters sum of squares for clusters of k value 1 to 15

for(i in 1:15){
  k[[i]] <- kmeans(Holiday_Data_num, i, iter.max = 100)
  betweenss[[i]] <- k[[i]]$betweenss
  totalss[[i]] <- k[[i]]$tot.withinss
  ratio[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
plot(1:15, ratio, type = 'b', ylab = 'Between SS / Total SS', xlab = "Clusters(k)")

#either 2 or 3 is the best value

#Using nbclust package to verify same

set.seed(1234)
nc <- NbClust(Holiday_Data_num, min.nc=2, max.nc=15, method="kmeans")


table(nc$Best.n[1,])


#K= 2 and 3 both shows maximum values, geneally an odd value for k is preffered and hence k=3 is optimumu value.
#Let's make model using both values and evaluate.

cluster_up <- kmeans(Holiday_Data_num, 2, iter.max = 100, nstart = 25)


fviz_cluster(cluster_up, data = Holiday_Data_num)

#Creating table of average of various columns for each cluster

aggregate(Holiday_Data[-1], by=list(cluster=cluster_up$cluster), mean)

#Not much insight can be gathered from 2 clusters it just divide data into two categories user with less reviews on all groups i.e. new users and people with #more reviews i.e outgoing or active users

#Lets crete cluster with more than 2 groups i.e. Building cluster with k=3

cluster_up <- kmeans(Holiday_Data_num, 3, iter.max = 100, nstart = 25)


#Analysing the cluster

print(cluster_up)


#Above output shows the sizes of three clusters formed along with the mean value for each column for each cluster. 
#Then the output displays a vector of cluster value for each row.
#Within cluster sum of squares is displayed for each of the three clusters.


str(cluster_up)

#Above output displays following information
#Clsuter:A vector of integers (from 1:k) indicating the cluster to which each point is allocated. 
#Centers:A matrix of cluster centers. 
#totss:The total sum of squares. 
#withinss:Vector of within-cluster sum of squares, one component per cluster. 
#tot.withinss:Total within-cluster sum of squares, i.e. sum(withinss). 
#betweenss:The between-cluster sum of squares, i.e. $totss-tot.withinss$. 
#size:The number of points in each cluster 


fviz_cluster(cluster_up, data = Holiday_Data_num)

#Creating table of average of various columns for each cluster

aggregate(Holiday_Data[-1], by=list(cluster=cluster_up$cluster), mean)

#Below traits about useers can be assumed from the cluster information above.
#Cluster 3 - New Users with less reviews
#Cluster 2 - Young Artistic Crowd - More into Nature, Theatre
#Cluster 1 - Family oriented person - More into Religious institutions, shopping

#Visualizing clusters for various combination of scatterplots

plot(Holiday_Data_num, col=cluster_up$cluster)


#Focusing on some major insights:
#Assuming less number of reviews by a user on a particular destination means user visit that destination group less number of times.
#People who visit more relegious institutions tends to do more shopping reviews as can be viewed from the plot below

plot(Holiday_Data[c("Shopping","Religious")], col=cluster_up$cluster, 
     main = "Number of User reviews for Shopping vs Relegious destinations")

#People who are nature lovers do less shopping as can be viewed from the plot below

plot(Holiday_Data[c("Shopping","Nature")], col=cluster_up$cluster,
     main = "Number of User reviews for Shopping vs Nature destinations")

#People who are nature lovers are more into Theatre destinations as can be viewed from the plot below

plot(Holiday_Data[c("Theatre","Nature")], col=cluster_up$cluster,
     main = "Number of User reviews for Theatre vs Nature destinations")


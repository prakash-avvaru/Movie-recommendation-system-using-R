library(recommenderlab)
library(ggplot2)                      
library(data.table)
library(reshape2)
setwd("E:/Bigdata_Project")
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")
str(movie_data)
str(rating_data)

#summary of movies
summary(movie_data)    
head(movie_data)

#summary of rating
summary(rating_data)
head(rating_data)

#Data Preprocessing
library(data.table)
# Split genres into a data frame
movie_genre2 <- as.data.frame(tstrsplit(movie_data$genres, '[|]', type.convert = TRUE), stringsAsFactors = FALSE)
colnames(movie_genre2) <- 1:ncol(movie_genre2)
list_genre <- c("Action", "Adventure", "Animation", "Children", "Comedy", 
                "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", 
                "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
# Create and populate the genre matrix
genre_mat2 <- matrix(0, nrow(movie_genre2), length(list_genre))
colnames(genre_mat2) <- list_genre

for (row in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    if (list_genre[col] %in% movie_genre2[row, col]) {
      genre_mat2[row, col] <- 1
    }
  }
}
genre_mat2 <- as.data.frame(genre_mat2, stringsAsFactors = FALSE)
str(genre_mat2)

SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])
head(SearchMatrix)    

library(reshape2)
ratingMatrix <- reshape2::dcast(rating_data, userId ~ movieId, value.var = "rating", na.rm = FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
library(recommenderlab)
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

#recommendation
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

lapply(recommendation_model, "[[", "description")
#parameters
recommendation_model$IBCF_realRatingMatrix$parameters

#exploring Similar data
similarity_mat <- as.matrix(similarity(ratingMatrix[1:4,],method="cosine",which="users"))
image(similarity_mat, main = "User's Similarities")

movie_similarity <- as.matrix(similarity(ratingMatrix[,1:4],method="cosine",which="items"))
image(movie_similarity, main = "Movies similarity")


#visualization of data
library(ggplot2)
movie_views <- colCounts(ratingMatrix)
table_views <- data.frame(movie = names(movie_views), views = movie_views)
table_views <- table_views[order(table_views$views, decreasing = TRUE), ]
table_views$title <- movie_data$title[match(table_views$movie, movie_data$movieId)]

table_views[1:6, ]


ggplot(table_views[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of the Top Films")


#Data preparation
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,colCounts(ratingMatrix) > 50]

minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,colCounts(movie_ratings) > minimum_users]
      ,main = "Heatmap of the top users and movies")

#visualize the distribution of the average ratings per user
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red"))+ggtitle("Distribution of the average rating per user")

#Data Normalization
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

#Performing Data Binarization
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
movies_watched <- binarize(movie_ratings, minRating = 1)

good_rated_films <- binarize(movie_ratings, minRating = 3)


#Create record that the user items i1 and i2.
#Calculate the similarity between i1 and i2.
#80% training set and 20% test set.
sampled_data<- sample(x = c(TRUE, FALSE),size = nrow(movie_ratings),replace = TRUE,prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

#Building the Recommendation System using R
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommen_model <- Recommender(data = training_data,method = "IBCF",parameter = list(k = 30))
recommen_model
class(recommen_model)



#carry out the sum of rows and columns with the similarity of the objects above 0.
#We will visualize the sum of columns through a distribution
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")


top_recommendations <- 10
predicted_recommendations <- predict(recommen_model, newdata = testing_data, n = top_recommendations)

user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                             movie_data$movieId == movies_user1[index])$title)
}
movies_user2


#plotting using the dimensions
recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:3]




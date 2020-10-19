# The idea of this capstone project is to make a movie recommendation system using the movielens dataset using all the 
# knowledge obtained on data science during the previous courses.

#########################################################
# Create edx set, validation set (final hold_out test set)
#########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(recosystem)
library(knitr)
library(gridExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

?read.table
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")
movielens

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
edx
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
validation
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#########################################################
# Exploratory analysis
#########################################################

# In the following an exploratory analysis of the 10M Movielens data is
# performed to inspect potential dependencies of the rating on the 
# information given in the data set. 

head(edx)
# the number of observations in the Movielens data set
length(edx$rating)+length(validation$rating)
# mean of ratings in the Movielens data set
mean_rating <- mean(edx$rating)
# rating distribution
edx %>% ggplot(aes(x=rating)) +
  geom_bar()+
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) + 
  geom_vline(xintercept = mean_rating, colour = "blue", linetype = "dashed") +
  theme_bw()
mean_rating
# NUMBER OF RATINGS PER VARIABLE
## Movies ##
# mean ratings per movie (boxplot)
movie_boxplot <-edx %>% group_by(title) %>%
  summarize(movieId= mean(movieId), mean_rating = mean(rating)) %>%
  ggplot(aes(x=mean_rating)) +
  geom_boxplot()+
  ggtitle("Movies")+
  coord_flip() +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) + 
  geom_vline(xintercept = mean_rating, colour = "blue", linetype = "dashed") +
  theme_bw()

#top 10 movies
edx %>% group_by(title) %>%
  summarize(movieId= mean(movieId), mean_rating = mean(rating)) %>%
  arrange(desc(mean_rating)) %>%
  top_n(10)

#bottom 10 movies
bottom_10 <- edx %>% group_by(title) %>%
  summarize(movieId= mean(movieId), mean_rating = mean(rating)) %>%
  arrange(mean_rating) %>%
  top_n(-10) 
bottom_10 <- bottom_10[1-10,]
bottom_10[seq(from = 1, to = 10), by = 1,]
kable(bottom_10)

?seq

# histogram per movie
edx %>% count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30) + 
  scale_x_log10() + 
  xlab("Frequency") +
  ylab("Rating") + 
  theme_bw()
# plot mean rating vs number of reviews per movie
edx %>% group_by(movieId) %>%
  summarize(number_reviews = n(), mean_rating = mean(rating))%>%
  ggplot(aes(x=number_reviews, y=mean_rating)) +
  geom_point() + 
  theme_bw()



## Users ##
# mean ratings per user (boxplot)
user_boxplot <- edx %>% group_by(userId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x=mean_rating)) +
  geom_boxplot()+
  ggtitle("Users")+
  coord_flip() +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) + 
  geom_vline(xintercept = mean_rating, colour = "blue", linetype = "dashed") +
  theme_bw()
# histogram per user
edx %>% group_by(userId) %>%
  summarize(number_reviews = n())%>%
  ggplot(aes(x=number_reviews)) +
  geom_histogram(binwidth = 10) + 
  scale_y_continuous(trans = 'log10') + 
  theme_bw()
# plot mean rating vs number of reviews per user
edx %>% group_by(userId) %>%
  summarize(number_reviews = n(), mean_rating = mean(rating))%>%
  ggplot(aes(x=number_reviews, y=mean_rating)) +
  geom_point() + 
  theme_bw()



##Genres##
# number of ratings for every genre
edx_split_genres  <- edx  %>% separate_rows(genres, sep = "\\|")
head(edx_split_genres)
edx_split_genres %>% group_by(genres) %>%
  summarize(number_reviews = n()) %>%
  ggplot(aes(x = reorder(genres, number_reviews), number_reviews)) + 
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Genres") +
  ylab("Number of ratings for each genre") + 
  theme_bw()
# number of movies for each genre
edx %>% select(movieId)%>%distinct()%>% summarise(n=n())
edx %>% group_by(movieId)%>%
  sample_n(size=1) %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(number_movies = n()) %>%
  ggplot(aes(x = reorder(genres, number_movies), number_movies)) + 
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Genres") +
  ylab("Number of movies for each genre") + 
  theme_bw()
# number of ratings per movie for each genre
edx %>% select(movieId)%>%distinct()%>% summarise(n=n())
n_movies_per_genre <- edx_split_genres %>% 
  select(genres,movieId) %>%
  group_by(genres) %>%
  distinct() %>%
  summarize(n_movies_per_genre=n())
edx_split_genres %>%
  group_by(genres) %>%
  summarize(number_reviews = n()) %>%
  left_join(n_movies_per_genre) %>%
  mutate(n_ratings_per_movie = number_reviews/n_movies_per_genre)%>%
  ggplot(aes(x = reorder(genres, n_ratings_per_movie), n_ratings_per_movie)) + 
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Genres") +
  ylab("Number of ratings per movie for each genre") + 
  theme_bw()
# confidence intervals for each genre
edx_split_genres %>%
  group_by(genres) %>%
  summarize(number_reviews = n(), mean_rating = mean(rating), sd_rating = sd(rating)) %>%
  filter(number_reviews > 8) %>%
  ggplot(aes(x = reorder(genres, mean_rating), y = mean_rating)) +
  geom_point() + 
  geom_errorbar(width=.5, aes(ymin=mean_rating-1.96*sd_rating, ymax=mean_rating+1.96*sd_rating)) +
  coord_flip()  +
  xlab("Genres") +
  ylab("95% confidence interval of ratings for each genre") + 
  theme_bw()
# mean ratings per genre (boxplot)
genre_boxplot <- edx_split_genres %>%
  group_by(genres) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x=mean_rating)) +
  geom_boxplot()+
  ggtitle("Genres")+
  coord_flip() +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5), limits = c(0.5, 5)) + 
  geom_vline(xintercept = mean_rating, colour = "blue", linetype = "dashed") +
  theme_bw()

##Time movie was released and age at rating##
edx_year <- edx %>% mutate(rate_year = year(as_datetime(timestamp)))
edx_year <- edx_year %>% mutate(title = str_replace(title,"^(.+)\\s\\((\\d{4})\\)$","\\1__\\2" )) %>% 
  separate(title,c("title","release_year"),"__") %>%
  select(-timestamp) 
edx_year <- edx_year %>% mutate(movieage_at_rating= as.numeric(rate_year)-as.numeric(release_year), release_year = as.numeric(release_year))
head(edx_year)
# mean ratings per release year (boxplot)
release_year_boxplot <- edx_year %>%
  group_by(release_year) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x=mean_rating)) +
  geom_boxplot()+
  ggtitle("Release_year")+
  coord_flip() +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5), limits = c(0.5, 5)) + 
  geom_vline(xintercept = mean_rating, colour = "blue", linetype = "dashed") +
  theme_bw()
# mean ratings per release year (lineplot)
edx_year %>%
  group_by(release_year) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(y=mean_rating, x=release_year)) +
  geom_point()+
  ggtitle("Release_year")+
  scale_y_continuous(breaks = seq(0.5, 5, 0.5), limits = c(0.5, 5)) +
  scale_x_continuous(breaks = seq(1900, 2020, by = 10), limits = c(NA,NA))+
  geom_hline(yintercept = mean_rating, colour = "blue", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme_bw()

# mean ratings per movieage at rating (boxplot)
movieage_boxplot <- edx_year %>%
  group_by(movieage_at_rating) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x=mean_rating)) +
  geom_boxplot()+
  ggtitle("movieage_at_rating")+
  coord_flip() +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5), limits = c(0.5, 5)) + 
  geom_vline(xintercept = mean_rating, colour = "blue", linetype = "dashed") +
  theme_bw()
movieage_boxplot

# mean ratings per movieage (scatterplot)
edx_year %>%
  group_by(movieage_at_rating) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(y=mean_rating, x=movieage_at_rating)) +
  geom_point()+
  ggtitle("movieage_at_rating")+
  scale_y_continuous(breaks = seq(0.5, 5, 0.5), limits = c(0.5, 5)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10))+
  geom_hline(yintercept = mean_rating, colour = "blue", linetype = "dashed")  +
  theme_bw()

##Summary##
grid.arrange(movie_boxplot, user_boxplot, genre_boxplot, release_year_boxplot, movieage_boxplot, nrow = 1)
??grid.arrange
#########################################################
# MOVIE PREDICTION ALGORITHMS AND RMSE CALCULATIONS
#########################################################

# In the following several computation algorithms are 
# tested to see which provides the best movie recommendations.
# The RMSE will be the parameter to evaluate the different algorithms
# by checking how the predicted rating for user u and movie i correspond
# to the actual rating the user gave that movie


# RMSE function
RMSE <- function(ratings_true, ratings_predicted){
  sqrt(mean((ratings_true - ratings_predicted)^2))
}

# Split edx data in training (80%) and test set (20%) 
set.seed(1992, sample.kind = "Rounding")
edx_test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
edx_train_set <- edx[-edx_test_index,]
edx_test_set <- edx[edx_test_index,]

edx_test_set <- edx_test_set %>%
  semi_join(edx_train_set, by = "movieId") %>%
  semi_join(edx_train_set, by = "userId")

# Model 1: average rating 
# average rating of all movies for all users to predict the rating for user u and movie i
model_1_prediction <- mean(edx_train_set$rating)
model_1_prediction

model_1_RMSE <- RMSE(edx_test_set$rating, model_1_prediction)
model_1_RMSE

all_RMSEs <- data_frame(method = "Model 1 : overall average", RMSE = model_1_RMSE)

# Model 2: average rating + movie bias
# inclusion of a movie bias (m_bias) since movies can in average be rated higher or lower than the overall average
avg_rating <- mean(edx_train_set$rating)

movie_avgs <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(m_bias = mean(rating - avg_rating))

model_2_prediction <- avg_rating + edx_test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  .$m_bias

model_2_RMSE <- RMSE(model_2_prediction, edx_test_set$rating)
all_RMSEs <- bind_rows(all_RMSEs,
                          data_frame(method="Model 2: movie bias",
                                     RMSE = model_2_RMSE))
all_RMSEs %>% knitr::kable()

# Model 3: average rating + movie bias + user bias
# inclusion of a user bias (u_bias) since user can in average rate higher or lower than the overall average
avg_rating <- mean(edx_train_set$rating)

user_avgs <- edx_train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(u_bias = mean(rating - avg_rating - m_bias))

model_3_prediction <- edx_test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(prediction = avg_rating + m_bias + u_bias) %>%
  .$prediction
  

model_3_RMSE <- RMSE(model_3_prediction, edx_test_set$rating)

all_RMSEs <- bind_rows(all_RMSEs,
                       data_frame(method="Model 3: movie bias + user bias",
                                  RMSE = model_3_RMSE))
all_RMSEs %>% knitr::kable()

# Model 4: average rating + regularized movie bias 
# inclusion of a regularized movie bias (reg_m_bias) to account for movies which were not rated often
# !!!!!!!Include regularization model!!!!!!!!!!!


#Optimize the lambda parameter in the regularization formula
lambdas <- seq(0, 10, 0.25)
avg_rating <- mean(edx_train_set$rating)
sum_rating_minus_avg <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - avg_rating), n_i = n())

RMSEs <- sapply(lambdas, function(l){
  predicted_ratings <- edx_test_set %>% 
    left_join(sum_rating_minus_avg, by='movieId') %>% 
    mutate(m_bias = s/(n_i+l)) %>%
    mutate(prediction = avg_rating + m_bias) %>%
    .$pred
  return(RMSE(predicted_ratings, edx_test_set$rating))
})
qplot(lambdas, RMSEs)
m_lambda <- lambdas[which.min(RMSEs)]

all_RMSEs <- bind_rows(all_RMSEs,
                       data_frame(method="Model 4: regularized movie bias",
                                  RMSE = min(RMSEs)))
all_RMSEs %>% knitr::kable()

# Model 5: regularized user bias
#Include a regularized user bias to lower the importance of the user bias for users who did not yet rate a lot of movies

#Optimize the lambda parameter in the regularization formula
lambdas <- seq(0, 10, 0.25)
avg_rating <- mean(edx_train_set$rating)
sum_rating_minus_avg <- edx_train_set %>% 
  group_by(userId) %>% 
  summarize(s = sum(rating - avg_rating), n_i = n())

RMSEs <- sapply(lambdas, function(l){
  predicted_ratings <- edx_test_set %>% 
    left_join(sum_rating_minus_avg, by='userId') %>% 
    mutate(u_bias = s/(n_i+l)) %>%
    mutate(prediction = avg_rating + u_bias) %>%
    .$pred
  return(RMSE(predicted_ratings, edx_test_set$rating))
})
qplot(lambdas, RMSEs)
u_lambda <- lambdas[which.min(RMSEs)]

all_RMSEs <- bind_rows(all_RMSEs,
                       data_frame(method="Model 5: regularized user bias",
                                  RMSE = min(RMSEs)))
all_RMSEs %>% knitr::kable()

# Model 6: regularized movie bias + user bias
#Now include both a regularized user bias with lambda 5.5 and a regularized movie bias with lambda 1.75
u_lambda <- 5.5
m_lambda <- 1.75

avg_rating <- mean(edx_train_set$rating)

# regularized movie bias based on the lambda calculated in model 4
reg_movie_bias <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(reg_m_bias = (sum(rating - avg_rating))/(n()+m_lambda))

# regularized user bias based on the lambda calculated in model 5
reg_user_bias <- edx_train_set %>% 
  left_join(reg_movie_bias, by='movieId') %>%
  group_by(userId) %>% 
  summarize(reg_u_bias = (sum(rating - avg_rating - reg_m_bias))/(n()+u_lambda))
 
model_6_prediction <- edx_test_set %>%
  left_join(reg_movie_bias, by='movieId') %>%
  left_join(reg_user_bias, by='userId') %>%
  mutate(prediction = avg_rating + reg_m_bias + reg_u_bias) %>%
  .$prediction

model_6_RMSE <- RMSE(model_6_prediction, edx_test_set$rating)
model_6_RMSE

all_RMSEs <- bind_rows(all_RMSEs,
                       data_frame(method="Model 6: regularized movie bias + regularized user bias",
                                  RMSE = model_6_RMSE))
all_RMSEs %>% knitr::kable()

# Model 7: regularized movie bias + user bias 2
# In the second type the lambda is simultaneously optimized for the movie bias and the user bias

#First optimize the lambda
avg_rating <- mean(edx_train_set$rating)
lambdas <- seq(0, 10, 0.25)

RMSEs <- sapply(lambdas, function(um_lambda){
  reg_movie_bias <- edx_train_set %>% 
    group_by(movieId) %>% 
    summarize(reg_m_bias = (sum(rating - avg_rating))/(n()+um_lambda))
  reg_user_bias <- edx_train_set %>% 
    left_join(reg_movie_bias, by='movieId') %>%
    group_by(userId) %>% 
    summarize(reg_u_bias = (sum(rating - avg_rating - reg_m_bias))/(n()+um_lambda))
  predicted_ratings <- edx_test_set %>%
    left_join(reg_movie_bias, by='movieId') %>%
    left_join(reg_user_bias, by='userId') %>%
    mutate(prediction = avg_rating + reg_m_bias + reg_u_bias) %>%
    .$prediction
  return(RMSE(predicted_ratings, edx_test_set$rating))
})

qplot(lambdas, RMSEs)
um_lambda <- lambdas[which.min(RMSEs)]
model_7_RMSE <- min(RMSEs)
all_RMSEs <- bind_rows(all_RMSEs,
                       data_frame(method="Model 7: regularized movie bias + regularized user bias 2",
                                  RMSE = model_7_RMSE))
all_RMSEs %>% knitr::kable()

#Predict the ratings using the lambda which was used to regularize user and movie bias simultaneously

# Model 8: Matrix factorization
# To further enhance the recommendation the residuals have to be modeled using matrix factorization,
# because many correlations cannot be found through logic deduction.
# The matrix factorization will be performed using the recosystem package which is a 
# recommender system based on matrix factorization
# Model 7 will be used as baseline system to calculate the residuals from
#??? or recommender lab package

# Obtain the residuals using the lambda of model 7
lambda <- 4.75
avg_rating <- mean(edx_train_set$rating)

reg_movie_bias <- edx_train_set %>% 
  group_by(movieId) %>% 
  summarize(reg_m_bias = (sum(rating - avg_rating))/(n()+lambda))
reg_user_bias <- edx_train_set %>% 
  left_join(reg_movie_bias, by='movieId') %>%
  group_by(userId) %>% 
  summarize(reg_u_bias = (sum(rating - avg_rating - reg_m_bias))/(n()+um_lambda))

predicted_ratings_m7 <- 
  edx_test_set %>% 
  left_join(reg_movie_bias, by = "movieId") %>%
  left_join(reg_user_bias, by = "userId") %>%
  mutate(pred = avg_rating + reg_m_bias + reg_u_bias)%>%
  pull(pred) 

residuals_train_set <- edx_train_set %>% 
  left_join(reg_movie_bias, by = "movieId") %>%
  left_join(reg_user_bias, by = "userId") %>%
  mutate(residual = rating - avg_rating - reg_m_bias - reg_u_bias) %>%
  select(userId, movieId, residual)
head(residuals)


# Use recosystem package to perform matrix factorization
install.packages("recosystem")
library(recosystem)
matrix_train_residuals <- residuals_train_set %>% as.matrix() 
matrix_test <- edx_test_set %>% 
  select(userId, movieId, rating) %>%
  as.matrix()

# write the matrices on disk and assign them to a variable
write.table(matrix_train_residuals , file = "matrixtrainresiduals.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(matrix_test, file = "matrix_test.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

set.seed(1992, sample.kind = "Rounding") 
trainset <- data_file("matrixtrainresiduals.txt")
testset <- data_file("matrix_test.txt")

# make a recommender object
recommender <-Reco()

# tuning the recommender with the training data
opts <- recommender$tune(trainset, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 1, niter = 10))
opts

# training the matrix factorization model

recommender$train(trainset, opts = c(opts$min, nthread = 1, niter = 20))

# prediction using trained model

prediction <- tempfile()
recommender$predict(testset, out_file(prediction))
prediction_resid_matfac <- scan(prediction)
prediction_ratings_matfac <- predicted_ratings_m7 + prediction_resid_matfac
model_8_RMSE <- RMSE(prediction_ratings_matfac, edx_test_set$rating)
model_8_RMSE
all_RMSEs <- bind_rows(all_RMSEs,
                       data_frame(method="Model 8: Matrix factorization",
                                  RMSE = model_8_RMSE))
all_RMSEs %>% knitr::kable()

## Final validation with model 8
# Now use complete edx set for training
#first use model 7 for regularized user and movie bias

lambda <- 4.75
avg_rating <- mean(edx$rating)

reg_movie_bias_valid <- edx %>% 
  group_by(movieId) %>% 
  summarize(reg_m_bias = (sum(rating - avg_rating))/(n()+lambda))
reg_user_bias_valid <- edx %>% 
  left_join(reg_movie_bias_valid, by='movieId') %>%
  group_by(userId) %>% 
  summarize(reg_u_bias = (sum(rating - avg_rating - reg_m_bias))/(n()+um_lambda))

predicted_ratings_validation <- 
  validation %>% 
  left_join(reg_movie_bias_valid, by = "movieId") %>%
  left_join(reg_user_bias_valid, by = "userId") %>%
  mutate(pred = avg_rating + reg_m_bias + reg_u_bias)%>%
  pull(pred) 

residuals_edx_set <- edx %>% 
  left_join(reg_movie_bias_valid, by = "movieId") %>%
  left_join(reg_user_bias_valid, by = "userId") %>%
  mutate(residual = rating - avg_rating - reg_m_bias - reg_u_bias) %>%
  select(userId, movieId, residual)
head(residuals)

matrix_edx_residuals <- residuals_edx_set %>% as.matrix() 
matrix_validation <- validation %>% 
  select(userId, movieId, rating) %>%
  as.matrix()

write.table(matrix_edx_residuals , file = "matrixedxresiduals.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(matrix_validation, file = "matrix_validation.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

set.seed(1992, sample.kind = "Rounding") 
edxset <- data_file("matrixedxresiduals.txt")
validationset <- data_file("matrix_validation.txt")

# make a recommender object
recommender_valid <-Reco()

# tuning the recommender with the training data
opts_valid <- recommender_valid$tune(edxset, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                               costp_l1 = 0, costq_l1 = 0,
                                               nthread = 1, niter = 10))
opts_valid

# training the matrix factorization model

recommender_valid$train(edxset, opts = c(opts_valid$min, nthread = 1, niter = 20))

prediction_validation <- tempfile()
recommender_valid$predict(validationset, out_file(prediction_validation))
prediction_validation_resid <- scan(prediction_validation)
prediction_validation_ratings <- predicted_ratings_validation + prediction_validation_resid
RMSE_validation <- RMSE(prediction_validation_ratings, validation$rating)
RMSE_validation

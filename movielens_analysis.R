#Ramesh K Patil
#MovieLens Rating Prediction
#http://github.com/patilram


################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#####Create edx set, validation set creation script worked fine whereas
##title and genres are marked as NA in created testsets.
##as per suggestion, downloaded edx and validation fromgoogle drive and processed accordingly
#edx = readRDS('edx.rds')
#validation = readRDS('validation.rds')
# Now we do have two datasets those can be used for modelling and analysis.

#Explore dataset to understand structure
str(edx)

#View dataset summary to understand details
summary(edx)

#View edx data in tidy format as a tibble
head(edx) %>% as_tibble()


# Count the number of unique users and movies in the dataset 
edx %>% summarize(num_users = n_distinct(userId), 
            num_movies = n_distinct(movieId))

#Analyse Ratings distribution in the edx dataset
edx %>% ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Ratings Distribution")


# Plot a Movies Distribution i.e. number of ratings per movie
edx %>%   count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Number of Movies") +
  ggtitle("Movies Distribution")

# Plot Users Distribution i.e. number of ratings given by users
edx %>% count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of Ratings") + 
  ylab("Number of Users") +
  ggtitle("Users Distribution")

# Plot Mean Movie Ratings Given by the Users
edx %>% group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(bias_u = mean(rating)) %>%
  ggplot(aes(bias_u)) +
  geom_histogram(bins = 30, color = "black") +
  xlab("Mean Rating") +
  ylab("Number of Users") +
  ggtitle("Mean movie Ratings Given by the Users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_light()

# List down the movies those are rated only once
edx %>% group_by(movieId) %>%
  summarize(counter = n()) %>%
  filter(counter == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(Rating = first(rating), num_rating = first(counter)) %>%
  slice(1:20) %>%
  knitr::kable()


#Root Mean Square Deviation (RMSE)
#function for vectors of movie ratings and predictors

RMSECalc <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2))
}

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#Model 1 : Predict average movie rating for all movies

# Compute the dataset's mean rating
mu <- mean(edx$rating)
mu

# Calculate RMSE using naive approach based on simple prediction
naive_rmse <- RMSECalc(validation$rating, mu)
naive_rmse

# Save rmse results in table
rmse_results <- tibble(method = "Average Movie Rating : Naive Approach", RMSE = naive_rmse)
rmse_results %>% knitr::kable()
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#Model 2:Movie Effect Modelling 

#estimate bias "bias_movies" for all of the movies
movie_avgs <- edx %>% group_by(movieId) %>%
  summarize(bias_movies = mean(rating - mu))
#plot movie bias "bias_movies"
movie_avgs %>% qplot(bias_movies, geom ="histogram", bins = 10, data = ., color = I("black"),
                     ylab = "Number ofMmovies", main = "Number of Movies with bias_movies")

#Predictions considering Movie bias
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(bias_movies)

#Calculate RMSE considering movie effect
model_movieeffect_rmse <- RMSECalc(predicted_ratings, validation$rating)
# Save results in table
rmse_results <- bind_rows(rmse_results, tibble(method="Movie Effect Model",  
                                     RMSE = model_movieeffect_rmse ))

rmse_results %>% knitr::kable()
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Model 3: Movie and User Effect modelling , so we will model User Effect in Model 2

#Estimate average ratings provided by users over 100 movies
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(bias_users = mean(rating - mu - bias_movies))
#Plot average ratings by users
user_avgs%>% qplot(bias_users, geom ="histogram", bins = 30, data = ., color = I("black"))

#estimating User bias "bias_users" for all of the users
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(bias_users = mean(rating - mu - bias_movies))


# Predict considering movie and user effect
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(prediction = mu + bias_movies + bias_users) %>%
  pull(prediction)

#Calculate RMSE considering Movie and Users effect in th model
model_movieusereffect_rmse <- RMSECalc(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie and User Effect Model",  
                                     RMSE = model_movieusereffect_rmse))

#Save the results to table
rmse_results %>% knitr::kable()
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#Model 4: Regularizing Movie and User Effect in Model 3 : Movie and User Effect

# lambda is a tuning parameter as penalty term
lambdas <- seq(0, 10, 0.25)


# Calculate bias_movies and bias_users for each lambda
  
rmses_reg <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  bias_movie <- edx %>% 
    group_by(movieId) %>%
    summarize(bias_movie = sum(rating - mu)/(n()+l))
  
  bias_user <- edx %>% 
    left_join(bias_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(bias_user = sum(rating - bias_movie - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(bias_movie, by = "movieId") %>%
    left_join(bias_user, by = "userId") %>%
    mutate(prediction = mu + bias_movie + bias_user) %>%
    pull(prediction)
  
  return(RMSECalc(predicted_ratings, validation$rating))
})


# Plot RMSEs vs lambdas to select the optimal lambda                                                             
qplot(lambdas, rmses_reg)  


# Now get the optimal lambda                                                             
lambda <- lambdas[which.min(rmses_reg)]
lambda

#Calculate RMSE considering regularized Movie and User Effect from previous Model 3: Movie and User Effect
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Movie and User Effect model",  
                                     RMSE = min(rmses_reg)))

#RMSE for regularized model
rmse_results %>% knitr::kable()
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Summary of RMSE results for all models looks like below                                                         
rmse_results %>% knitr::kable()

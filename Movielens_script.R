#####Setup#####

#Install the required packages
if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(stringr)) 
  install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) 
  install.packages("lubridate", repos = "http://cran.us.r-project.org")

#Load the required packages
library(tidyverse)
library(ggthemes)
library(stringr)
library(lubridate)

#The code below loads the data on my system.
#Replace the below filepath with the filepath for these files on your system to load the dataset

edx <- readRDS("C:\\Users\\Rahul Saran\\Desktop\\Data Science\\Capstone\\edx.rds")
validation <- readRDS("C:\\Users\\Rahul Saran\\Desktop\\Data Science\\Capstone\\validation.rds")

#The code below is an alternative to download and loads the data. Do not use this if using R 3.6.0.
#If using this code, please uncomment it (remove one hashtag from each line of the code).

###################################
## Create edx set and validation set
###################################

## Note: this process could take a couple of minutes

#if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
#if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

## MovieLens 10M dataset:
## https://grouplens.org/datasets/movielens/10m/
## http://files.grouplens.org/datasets/movielens/ml-10m.zip

#dl <- tempfile()
#download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                      col.names = c("userId", "movieId", "rating", "timestamp"))

#movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
#colnames(movies) <- c("movieId", "title", "genres")
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

#movielens <- left_join(ratings, movies, by = "movieId")

## Validation set will be 10% of MovieLens data

#set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
#test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
#edx <- movielens[-test_index,]
#temp <- movielens[test_index,]

## Make sure userId and movieId in validation set are also in edx set

#validation <- temp %>% 
#     semi_join(edx, by = "movieId") %>%
#     semi_join(edx, by = "userId")

## Add rows removed from validation set back into edx set

#removed <- anti_join(temp, validation)
#edx <- rbind(edx, removed)

#rm(dl, ratings, movies, test_index, temp, movielens, removed)



#####Exploratory Data Analysis & Visualization#####

#Top-level look at data
head(edx)
str(edx)
dim(edx)
dim(validation)


summary(edx$rating) #top-level look

ratingc <- edx %>% group_by(rating) %>% summarize(n = n(), proportion = n/nrow(edx)) 
#this creates, for each value of rating, the proportion of movies rated at the value.

#the below is a graphical representation of the above
ratingc %>% ggplot(aes(rating, proportion)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  ylim(c(0,0.5)) +
  coord_flip() +
  geom_text(label = round(ratingc$proportion, 2), hjust = -0.5, color = "black", size = 3) + 
  theme_classic() + 
  ggtitle("Rating")


class(edx$userId)
length(unique(edx$userId)) #see number of unique users


userc <- edx %>% group_by(userId) %>% summarize(ratings = n())
#this gives the number of ratings given by each user.
#since there are ~70000 users, it is not useful to see a graphical representation
#of 70000 different values. Instead, it is useful to group users into buckets
#wherein each bucket has users who have given a similar number of ratings.

#the below code creates 4 user buckets - those who have given <10 ratings, 
#those who have given 10-50, those who have given 50-100, and those
#who have given more than 100.
userc <- mutate(userc, no_ratings = 
                  ifelse(ratings < 10, "<10", 
                         ifelse(ratings < 50, "<50", 
                                ifelse(ratings < 100, "<100", ">=100"))))

#the below code gives the number of users in each bucket.
userc2 <-userc %>% group_by(no_ratings) %>% summarize (users = n())

#this code makes a graphical representation of this information.
userc2 %>%
  ggplot(aes(no_ratings, users)) +
  geom_bar(stat = "identity") +
  ylim(c(0,40000)) +
  geom_text(label = userc2$users, vjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = c("<10","<50","<100", ">=100")) +
  theme_classic() + 
  ggtitle("Number of ratings given by users")

userc3 <- edx %>% group_by(userId) %>% summarize(avgrating = round(mean(rating),1))
#this code gives the average rating by each user Id.
#Now since there are ~70000 users, it is not informative to see a graph of average rating
#by each user. Instead, it is useful to see number of users who have given a particular
#average rating.

#The below code calculates number of users who have a particular average rating.
userc4 <- userc3 %>% group_by(avgrating) %>% summarize(users = n())

#The below code makes a graph of this information
userc4 %>%
  ggplot(aes(avgrating, users)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  ggtitle("Avg ratings given by users")


class(edx$movieId)
length(unique(edx$movieId))


moviec <- edx %>% group_by(movieId) %>% summarize(ratings = n())
#this gives the number of ratings given by each movie.
#since there are ~10000 users, it is not useful to see a graphical representation
#of 10000 different values. Instead, it is useful to group movies into buckets
#wherein each bucket has movies who have received a similar number of ratings.

#the below code creates 4 movie buckets - those who have got <25 ratings, 
#those who have got 25-100, those who have got 100-500, and those
#who have got more than 500.

moviec <- mutate(moviec, no_ratings = 
                   ifelse(ratings < 25, "<25", 
                          ifelse(ratings < 100, "<100", 
                                 ifelse(ratings < 500, "<500", ">=500"))))

#the below code gives the number of movies in each bucket.
moviec2 <-moviec %>% group_by(no_ratings) %>% summarize (movies = n())

#The below code makes a graph of this information
moviec2 %>%
  ggplot(aes(no_ratings, movies)) +
  geom_bar(stat = "identity") +
  ylim(c(0,5000)) +
  geom_text(label = moviec2$movies, vjust = -0.5, color = "black", size = 4) + 
  scale_x_discrete(limits = c("<25","<100","<500", ">=500")) +
  theme_classic() + 
  ggtitle("Number of ratings given to movies")


moviec3 <- edx %>% group_by(movieId) %>% summarize(avgrating = round(mean(rating),1))
#this code gives the average rating by each movie Id.
#Now since there are ~10000 movies, it is not informative to see a graph of average rating
#by each movie. Instead, it is useful to see number of movies who have got a particular
#average rating.

#The below code calculates number of movies who have got a particular average rating.
moviec4 <- moviec3 %>% group_by(avgrating) %>% summarize(movies = n())

#The below code makes a graph of this information
moviec4 %>%
  ggplot(aes(avgrating, movies)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  ggtitle("Avg ratings given to movies")


class(edx$timestamp)
length(unique(edx$timestamp))

edx <- mutate(edx, date = as_datetime(timestamp))
validation <- mutate(validation, date = as_datetime(timestamp))
head(edx$date)

min(edx$date)
max(edx$date)

edx <- mutate(edx, dateY = round_date(date, unit = "year"))
#this converts each date to the nearest year

dateYc <- edx %>% group_by(dateY) %>% summarize(ratings_in_thousand = n()/(10^3))
#this gives the number of ratings given in each year.

#this makes a chart of this information
dateYc %>% 
  ggplot(aes(dateY, ratings_in_thousand)) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  ggtitle("Ratings by Time of Rating")

dateYc2 <- edx %>% group_by(dateY) %>% summarize(avgrating = mean(rating))
#this gives the average rating given in each year.

#this makes a chart of this information.
dateYc2 %>% 
  ggplot(aes(dateY, avgrating)) + 
  geom_line() +
  ylim(c(0,5)) +
  theme_classic() + 
  ggtitle("Avg Rating by Time of Rating")


class(edx$title)
length(unique(edx$title))

#this code separates out the year, which is always the 5th-last to 2nd-last characters in title.
edx <- mutate(edx, 
              year = as.numeric(substr(edx$title, 
                                       nchar(edx$title)-4, nchar(edx$title)-1)))
validation <- mutate(validation, 
                     year = as.numeric(substr(validation$title, 
                                              nchar(validation$title)-4, 
                                              nchar(validation$title)-1)))
head(edx$year)

min(edx$year)
max(edx$year)

yearc <- edx %>% group_by(year) %>% summarize(ratings_in_thousand = n()/(10^3))
#this code gives the number of ratings by each year.

#this code makes a graph of this information
yearc %>% 
  ggplot(aes(year, ratings_in_thousand)) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  ggtitle("Ratings by Year of Release")

yearc2 <- edx %>% group_by(year) %>% summarize(avgrating = mean(rating))
#this code gives the average rating by each year

#this code makes a graph of this information
yearc2 %>% 
  ggplot(aes(year, avgrating)) + 
  geom_line() +
  ylim(c(0,5)) +
  theme_classic() + 
  ggtitle("Avg Rating by Year of Release")


class(edx$genres)
head(edx$genres)
length(unique(edx$genres))

genrec <- edx %>% group_by(genres) %>% summarize(ratings_in_thousand = n()/(10^3))
#this gives the number of ratings for each value of 'genres'

#this code gives a graph of this information
genrec %>% 
  ggplot(aes(genres, ratings_in_thousand)) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  ggtitle("Ratings by Genre")

# the following code breaks down the data for a movie into a separate row for each 'genre' 
# in the 'genres' variable. However, it may lead to crashing of the computer.
# edx %>% separate_rows(genres, sep = "\\|")

#the below code identifies whether the given text, example "Action", is part of the character
#list in the 'genres' column.

#edx <- mutate(edx, 
#                   Action = grepl("Action", genres, fixed = TRUE), 
#                   Adventure = grepl("Adventure", genres, fixed = TRUE), 
#                   Animation = grepl("Animation", genres, fixed = TRUE), 
#                   Children = grepl("Children", genres, fixed = TRUE),
#                   Comedy = grepl("Comedy", genres, fixed = TRUE),
#                   Crime = grepl("Crime", genres, fixed = TRUE),
#                   Drama = grepl("Drama", genres, fixed = TRUE),
#                   Fantasy = grepl("Fantasy", genres, fixed = TRUE),
#                   FilmNoir = grepl("Film-Noir", genres, fixed = TRUE),
#                   Horror = grepl("Horror", genres, fixed = TRUE),
#                   Musical = grepl("Musical", genres, fixed = TRUE),
#                   Mystery = grepl("Mystery", genres, fixed = TRUE),
#                   Romance = grepl("Romance", genres, fixed = TRUE),
#                   SciFi = grepl("Sci-Fi", genres, fixed = TRUE),
#                   Thriller = grepl("Thriller", genres, fixed = TRUE),
#                   War = grepl("War", genres, fixed = TRUE),
#                   Western = grepl("Western", genres, fixed = TRUE)
# )


#####Modelling#####

#create a loss function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}

#create a results table
rmse_results <- data.frame(method = character(), RMSE = numeric())

#predict all as average rating
mu <- mean(edx$rating)
mu

rmse_1 <- RMSE(edx$rating, mu)
rmse_1
rmse_results <- data.frame(method = "Mean Rating", RMSE = rmse_1)
rmse_results %>% knitr::kable()

#add movie variation
movie_var <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + edx %>% 
  left_join(movie_var, by = "movieId") %>% 
  .$b_i

rmse_2 <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method = "Movie Variation", RMSE = rmse_2))
rmse_results %>% knitr::kable()

#add user variation
user_var <- edx %>%
  left_join(movie_var, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- edx %>% 
  left_join(movie_var, by = "movieId") %>% 
  left_join(user_var, by = "userId") %>% 
  mutate(pred = mu + b_i + b_u) %>% .$pred

rmse_3 <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method = "Movie + User Variation", RMSE = rmse_3))
rmse_results %>% knitr::kable()

edx <- mutate(edx, dateW = round_date(date, unit = "week"))
validation <- mutate(validation, dateW = round_date(date, unit = "week"))

#add time variation
week_var <- edx %>%
  left_join(movie_var, by = "movieId") %>% 
  left_join(user_var, by = "userId") %>%
  group_by(dateW) %>% 
  summarize(b_t = mean(rating - mu - b_i - b_u))

predicted_ratings <- edx %>% 
  left_join(movie_var, by = "movieId") %>% 
  left_join(user_var, by = "userId") %>% 
  left_join(week_var, by = "dateW") %>%
  mutate(pred = mu + b_i + b_u + b_t) %>% .$pred

rmse_4 <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method = "Movie + User + Time Variation", RMSE = rmse_4))
rmse_results %>% knitr::kable()

#add year variation
year_var <- edx %>%
  left_join(movie_var, by = "movieId") %>% 
  left_join(user_var, by = "userId") %>%
  left_join(week_var, by = "dateW") %>%
  group_by(year) %>% 
  summarize(b_y = mean(rating - mu - b_i - b_u - b_t))

predicted_ratings <- edx %>% 
  left_join(movie_var, by = "movieId") %>% 
  left_join(user_var, by = "userId") %>% 
  left_join(week_var, by = "dateW") %>%
  left_join(year_var, by = "year") %>%
  mutate(pred = mu + b_i + b_u + b_t + b_y) %>% .$pred

rmse_5 <- RMSE(predicted_ratings, edx$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method = "Movie + User + Time + Year Variation", 
                                     RMSE = rmse_5))
rmse_results %>% knitr::kable()


#this code applies the entire set of processes carried out in the model together.

mu_val <- mean(validation$rating)

movie_var_val <- validation %>% 
  group_by(movieId) %>% 
  summarize(b_i_val = mean(rating - mu_val))

user_var_val <- validation %>%
  left_join(movie_var_val, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u_val = mean(rating - mu_val - b_i_val))

week_var_val <- validation %>%
  left_join(movie_var_val, by = "movieId") %>% 
  left_join(user_var_val, by = "userId") %>%
  group_by(dateW) %>% 
  summarize(b_t_val = mean(rating - mu_val - b_i_val - b_u_val))

year_var_val <- validation %>%
  left_join(movie_var_val, by = "movieId") %>% 
  left_join(user_var_val, by = "userId") %>%
  left_join(week_var_val, by = "dateW") %>%
  group_by(year) %>% 
  summarize(b_y_val = mean(rating - mu_val - b_i_val - b_u_val - b_t_val))

predicted_ratings <- validation %>% 
  left_join(movie_var_val, by = "movieId") %>% 
  left_join(user_var_val, by = "userId") %>% 
  left_join(week_var_val, by = "dateW") %>%
  left_join(year_var_val, by = "year") %>%
  mutate(pred = mu_val + b_i_val + b_u_val + b_t_val + b_y_val) %>% .$pred

rmse_final <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method = "Final - Test", RMSE = rmse_final))
rmse_results %>% knitr::kable()
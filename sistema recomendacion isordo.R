# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(kableExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

#dataset visualization

head(edx)

# unique movies, users and genres

edx %>% summarise(
  uniq_movies = n_distinct(movieId),
  uniq_users = n_distinct(userId),
  uniq_genres = n_distinct(genres))

#rating mean

media<-mean(edx$rating)
media


#movies valoration

summary(edx$rating)

#movies number by valoration

table_ratings<-table(edx$rating)
table_ratings

#graphics
ggplot(edx,aes(rating))+
  geom_bar(width =0.50,fill="lightgreen", color="black")+
  scale_x_continuous(breaks = seq(0, 5, by=0.5))+
  labs(title= "Movies Rating")


#Distribution of Users
edx %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "lightgreen",color="black", bins = 10) +
  scale_x_log10() + 
  ggtitle("Number of Users Ratings")

#Distribution of Movie Ratings
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "lightgreen", color = "black", bins = 10) +
  scale_x_log10() +
  ggtitle("Number of Movies Ratings")



#most rated movies

conteo<-data.frame(table(edx$title))
names(conteo)<-c("Title","Times")

conteo<-conteo[order(conteo$Times, decreasing=TRUE),]

head (conteo,10)


#most rated movies graphics

ggplot(conteo[1:10, ], aes(x= reorder(Title,-Times), y=Times))+
  geom_bar(stat="identity", fill="lightgreen",color="black")+
  geom_text(aes(label=Times),vjust=-0.3, size=3.5)+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  ggtitle("Total views of the Top Movies")


#less rated movies

conteo<-data.frame(table(edx$title))
names(conteo)<-c("Title","Times")

conteo<-conteo[order(conteo$Times, decreasing=FALSE),]

head (conteo,10)

#Data analysis

# mean rating
media <- mean(edx$rating)
media

#testing in validation set

model_number0 <- RMSE(validation$rating, media)
model_number0



# calculate b_i using the training set 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - media))



# predicted ratings
predicted_ratings_bi <- media + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

#testing in validation set
model_number1 <- RMSE(validation$rating,predicted_ratings_bi)  
model_number1



#b.movie + user effect modelo

#calculate b_u using the training set 

user_avgs <- edx %>%  
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - media - b_i))

#predicted ratings
predicted_ratings_bu <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = media + b_i + b_u) %>%
  .$pred

#testing in validation set
model_number2 <- RMSE(validation$rating,predicted_ratings_bu)
model_number2


#c.movie + user + time effect

# previously create a copy of validation set , valid, and create the date feature which is the timestamp converted to a datetime object  and  rounded by week.

validation_copy <- validation
validation_copy <- validation_copy %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) 

# calculate time effects ( b_t) using the training set
temp_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(b_t = mean(rating - media - b_i - b_u))

# predicted ratings
predicted_ratings_bt <- validation_copy %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(temp_avgs, by='date') %>%
  mutate(pred = media + b_i + b_u + b_t) %>%
  .$pred

#testing in validation set
model_number3 <- RMSE(validation_copy$rating,predicted_ratings_bt)
model_number3


#regularization movie + user effect model

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - media)/(n()+l))
  
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - media)/(n()+l))
  
  predicted_ratings <-
    validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = media + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
})


#graphic lambda
qplot(lambdas, rmses)
lambdas[which.min(rmses)]


#testing in validation set
model_number4 <- min(rmses)
model_number4

#Total Results

resultados_totales <- data.frame(methods=c("Simple media","Movie effect (b_i)","Movie + user effects (b_u)","Movie + user + time effects (b_t)", "Regularized Movie + User Effect Model"),
                                 rmse = c(model_number0, model_number1, model_number2, model_number3, model_number4))

kable(resultados_totales) %>%
  kable_styling(bootstrap_options = "striped" , full_width = F , position = "center") %>%
  kable_styling(bootstrap_options = "bordered", full_width = F , position ="center") %>%
  column_spec(1,bold = T ) %>%
  column_spec(2,bold =T ,color = "black" , background ="lightgreen")





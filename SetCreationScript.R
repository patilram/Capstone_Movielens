
#####Create edx set, validation set
##title and genres are showed up as NA in dataset once generated.
##as per suggestion, downloaded edx and validation fromgoogle drive

edx = readRDS('edx.rds')
##============================================
####SECTION 1 : QUESTIONS AND ANSWERS
##============================================
summary(edx)    ###Look at summary of edx dataset
str(edx)    ### Look at structure of edx dataset

head(edx)
##1. How many rows and columns are there in the edx dataset?
dim(edx)

##2.How many zeros were given as ratings in the edx dataset?
edx %>% filter(rating ==0) %>%tally()
#OR
count(edx %>% filter(rating ==0))


##3. How many threes were given as ratings in the edx dataset?
edx %>% filter(rating ==3) %>%tally()
#OR
count(edx %>% filter(rating ==3))

##4. How many different movies are in the edx dataset?
length(unique(edx$movieId))

##5. How many different users are in the edx dataset?
length(unique(edx$userId))


##6. How many movie ratings are in each of the following genres in the edx dataset?
edx %>% separate_rows(genres, sep = ";") %>% group_by(genres) %>% summarize(counter = n()) %>% arrange(desc(counter))

##7. Which movie has the greatest number of ratings?
edx %>% group_by(movieId, title) %>% summarize(counter = n()) %>% arrange(desc(counter))

##8. What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(counter = n()) %>% top_n(5) %>% arrange(desc(counter))

##9. TRue/False  In General, half star ratings are less common than whole start ratings
edx %>% group_by(rating) %>% summarize(counter = n()) %>% ggplot(aes(x = rating, y = counter)) + geom_line()
edx
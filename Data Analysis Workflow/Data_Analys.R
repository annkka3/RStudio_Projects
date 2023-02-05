# Creating An Efficient Data Analysis Workflow

library(tidyverse)
reviews <- read_csv("book_reviews.csv")

# Getting Familiar With The Data

#How big is the dataset?
dim(reviews)

#[1] 2000  4

columns <- colnames(reviews)
columns

#[1] "book"   "review" "state"  "price" 

for (column in columns) {
  print(column)
  print(typeof(reviews[[column]]))
}

#[1] "book" - "character"
#[1] "review" - "character"
#[1] "state" - "character"
#[1] "price" - "double"


# What are the unique values in each column?
for (column in columns) {
  print("Unique values in the column:")
  print(column)
  print(unique(reviews[[column]]))
  print("")
}

# Handling Missing Data

#From the previous exercise, it's apparent that that the `review` column contains some `NA` values. We don't want any missing values in the dataset, so we need to get rid of them.

for (column in columns) {
  print(column)
  print(sum(is.na(reviews[[column]])))
}

#[1] "book"
#[1] 0
#[1] "review"
#[1] 206
#[1] "state"
#[1] 0
#[1] "price"
#[1] 0

# As we can see we have 206 NA values in 2nd column 'review', let's make a new dataset without NA

complete_reviews = reviews %>% 
  filter(!is.na(review))

dim(complete_reviews)
#[1] 1794    4

#The next thing that we need to work on is the state column. 
#You may have noticed that the labeling for each state is inconsistent.
#For example, California is written as both "California" and "CA". 
#Both "California" and "CA" refer to the same place in the United States, so we should try to clean this up.

complete_reviews <- complete_reviews %>% 
  mutate(
    state = case_when(
      state == "California" ~ "CA",
      state == "New York" ~ "NY",
      state == "Texas" ~ "TX",
      state == "Florida" ~ "FL",
      TRUE ~ state # ignore cases where it's already postal code
    )
  )

#Transforming The Review Data

#The first things we'll handle in the dataset are the reviews themselves. 
#You may have noticed in our data exploration that the reviews take the form of strings,
#ranging from "Poor" to "Excellent". Our goal is to evaluate the ratings of each of the textbooks, 
#but there's not much we can do with text versions of the review scores. 
#It would be better if we were to convert the reviews into a numerical form.



complete_reviews <- complete_reviews %>% 
  mutate( 
    review_num = case_when (
      review == "Poor" ~ 1,
      review == "Fair" ~ 2,
      review == "Good" ~ 3,
      review == "Great" ~ 4,
      review == "Excellent" ~ 5),
    is_high_review = if_else(review_num >= 4, T, F)
  )

#Analyzing The Data

#With all of our data cleaning, now we're ready to do some analysis of the data. 
#Our main goal is to figure out what book is the most profitable. 
#How will we judge what the "most profitable" book is though? 
#Our dataset represents customer purchases. 
#One way to define "most profitable" might be to just choose the book that's purchased the most. 
#Another way to define it would be to see how much money each book generates overall.


purchases <- complete_reviews %>%
  group_by(book) %>% 
  summarise(
    purchased = n() 
  ) %>% 
  arrange(-purchased)





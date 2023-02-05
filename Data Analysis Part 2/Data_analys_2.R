# Creating An Efficient Data Analysis Workflow, Part 2

#The company has provided us more data on some of its 2019 book sales, and it wants us to extract some usable knowledge from it.
#It launched a new program encouraging customers to buy more books on July 1st, 2019, and it wants to know if this new program was successful at increasing sales and improving review quality.
#As the analyst, this will be your job to figure out for the guided project.

data <- read_csv('sales2019.csv')

#How big is the dataset? What are the column names and what do they represent?

dim(data)

#[1] 5000    5

columns <-colnames(data)
columns

#[1] "date"  "user_submitted_review" "title" "total_purchased" "customer_type"  
  
#What are the types of each of the columns?

for (col in columns) {
    paste0(col, " : ", typeof(data[[col]])) %>% print
}

#[1] "date : character"
#[1] "user_submitted_review : character"
#[1] "title : character"
#[1] "total_purchased : double"
#[1] "customer_type : character"

#Do any of the columns have missing data? If so, make a note of this.

for (col in columns) {
  paste0(col, " : ", sum(is.na(data[[col]]))) %>% print
}

#[1] "date : 0"
#[1] "user_submitted_review : 885"
#[1] "title : 0"
#[1] "total_purchased : 718"
#[1] "customer_type : 0"


#The `user_submitted_review` column has some missing data in it. 
#We'll have to handle this later in the data cleaning, but at least we know about it ahead of time. 
#The `total_purchased` column also has missing data, which we'll handle with imputation.

# Handling Missing Data

# Remove the rows with no user_submitted_review
complete_data <- data %>% 
  filter(
    !is.na(user_submitted_review)
  )

dim(complete_data)
#[1] 4115    5

# Calculate the mean of the total_purchased column, without the missing values

mean_purchase <- mean(complete_data$total_purchased, na.rm = T)
mean_purchase

#[1] 3.985561

complete_data <- complete_data %>%
  mutate(
    full_purchases = if_else(is.na(total_purchased), 
                                mean_purchase,
                                total_purchased)
  )

# Processing Review Data


complete_data %>% pull(user_submitted_review) %>% unique

#[1] "it was okay"                          "Awesome!"                             "Hated it"                            
#[4] "Never read a better book"             "OK"                                   "The author's other books were better"
#[7] "A lot of material was not needed"     "Would not recommend"                  "I learned a lot"       

is_positive <- function(review) {
  review_positive = case_when(
    str_detect(review, "Awesome") ~ TRUE,
    str_detect(review, "OK") ~ TRUE,
    str_detect(review, "Never") ~ TRUE,
    str_detect(review, "a lot") ~ TRUE,
    TRUE ~ FALSE # The review did not contain any of the above phrases
  )
}
complete_data <- complete_data %>% 
  mutate(
    is_positive = unlist(map(user_submitted_review, is_positive))
  )

# Comparing Book Sales Between Pre- and Post-Program Sales

#With the review data and order quantities processed into a usable form, we can finally make a move towards answering the main question of the analysis,
#Was the new book program effective in increasing book sales? The program started on July 1, 2019 and the data have contains all of the sales for 2019.


complete_data <- complete_data %>% 
  mutate(
    date_status = if_else(mdy(date) < ymd("2019/07/01"), "Pre", "Post")
  )
complete_data %>% 
  group_by(date_status) %>% 
  summarize(
    books_purchased = sum(full_purchases)
  )

# A tibble: 2 × 2
#date_status books_purchased
#<chr>                 <dbl>
#  1 Post                  8190.
#  2 Pre                   8211.

#It doesn't seem that the program has increased sales. Maybe there were certain books that increased in sales?

complete_data %>% 
  group_by(date_status, title) %>% 
  summarize(
    books_purchased = sum(full_purchases)
  ) %>% 
  arrange(title, date_status)

# A tibble: 12 × 3
# Groups:   date_status [2]
#date_status title                              books_purchased
#<chr>       <chr>                                        <dbl>
#1 Post        Fundamentals of R For Beginners              2832.
#2 Pre         Fundamentals of R For Beginners              3093.
#3 Post        R For Dummies                                2779.
#4 Pre         R For Dummies                                2626.
#5 Post        R Made Easy                                    24 
#6 Pre         R Made Easy                                    15 
#7 Post        R vs Python: An Essay                        1172.
#8 Pre         R vs Python: An Essay                        1271.
#9 Post        Secrets Of R For Advanced Students           1154.
#10 Pre         Secrets Of R For Advanced Students            965.
#11 Post        Top 10 Mistakes R Beginners Make              228.
#12 Pre         Top 10 Mistakes R Beginners Make              241.

#It turns out that certain books actually got more popular after the program started!
#R For Dummies and Secrets of R For Advanced Students got more popular.


# Comparing Book Sales Within Customer Type

complete_data %>% 
  group_by(date_status, customer_type) %>% 
  summarize(
    books_purchased = sum(full_purchases)
  ) %>% 
  arrange(customer_type, date_status)

# A tibble: 4 × 3
# Groups:   date_status [2]
#date_status customer_type books_purchased
#<chr>       <chr>                   <dbl>
#1 Post        Business                5742.
#2 Pre         Business                5612.
#3 Post        Individual              2448.
#4 Pre         Individual              2599.


#Based on the table, it looks like businesses started purchasing more books after the program! 
#There was actually a drop in individual sales.


# Comparing Review Sentiment Between Pre- and Post-Program Sales

complete_data %>% 
  group_by(date_status) %>% 
  summarize(
    num_positive_reviews = sum(is_positive)
  )

# A tibble: 2 × 2
#date_status num_positive_reviews
#<chr>                      <int>
#1 Post                         903
#2 Pre                          900
#There's slightly more reviews before the program, but this difference seems negigible.



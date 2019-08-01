##### CODE GIVEN #####
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data
names(foo)
dim(foo)
head(foo)
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)
for(i in date.columns)  # this "for loop" only loops through the "date.columns" -- no other columns.

  {
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  foo[which_values_are_missing, i] <- NA
  foo[, i] <- as.Date(as.character(foo[, i]))

  }

##### MY CODE STARTS HERE #####

# first filtering out the rows that have CirculationDate as NA, which will be called new_foo
which.have.NAs <- which(is.na(foo$CirculationDate == TRUE))
new_foo <- foo[-which.have.NAs, ]

# filtering for projects with circulation date after 2008-01-01, which will be called work_foo
which.after.2008 <- which(new_foo$CirculationDate >= as.Date("2008-01-01"))
work_foo <- new_foo[which.after.2008, ]

### For problem 1
## Part 1

# taking out rows with original completion dates as NA, which is called foo_question1
which.have.NAs.OCD <- which(is.na(work_foo$OriginalCompletionDate == TRUE))
foo_question1 <- work_foo[-which.have.NAs.OCD, ]

# calculating project duration (original completion date - approval date)
proj_duration <- c(foo_question1$OriginalCompletionDate - foo_question1$ApprovalDate)

# calculating mean, median, and quartile for project duration
mean(proj_duration)
median(proj_duration)
quantile(proj_duration)

## Part 2

# calculating new project duration (revised completion date - approval date)
proj_duration_revised <- c(foo_question1$RevisedCompletionDate - foo_question1$ApprovalDate)

# calculating mean, median, and quartile for revised project duration
mean(proj_duration_revised)
median(proj_duration_revised)
quantile(proj_duration_revised)

### Problem 2 

# taking out rows with ratings as NA, which is called foo_question2
which.have.NAs.rating <- which(is.na(work_foo$Rating == TRUE))
foo_question2 <- work_foo[-which.have.NAs.rating, ]

# getting # of occurances for each rating for projects in foo_question2
table(foo_question2$Rating)

### Problem 3

# taking out rows with type as PPTA, which is called foo_no_ppta
which.are.ppta <- which(foo_question2$Type == "PPTA")
foo_no_ppta <- foo_question2[-which.are.ppta, ]

# getting # of occurances for each rating for projects in foo_no_ppta
table(foo_no_ppta$Rating)

### Problem 4

# get rows with top 25% budget (revisedamount) using order function
# There are 1869 occurrances total in foo_question2, so 25% would be equal to 467 rows
top.25 <- order(foo_question2$RevisedAmount, decreasing = TRUE)[1:467]
foo_top_25 <- foo_question2[top.25, ]

# do the same, with the bottom 25%
bottom.25 <- order(foo_question2$RevisedAmount, decreasing = FALSE)[1:467]
foo_bottom_25 <- foo_question2[bottom.25, ]

# get the mean rating for both top and bottom groups
mean(foo_top_25$Rating)
mean(foo_bottom_25$Rating)

# get mean budget for both top and bottom groups
mean(foo_top_25$RevisedAmount)
mean(foo_bottom_25$RevisedAmount)

# get summaries of both top and bottom groups to compare # of occurances in different columns
summary(foo_top_25)
summary(foo_bottom_25)
 @prancespak

#  Description of the performed analysis


First we load dplyr and tidyr packages:
```{r}
library(dplyr)
library(tidyr)
```
Then we read all necessary input files (features.txt, X_test.txt, y_test.txt, X_train.txt, y_train.txt, subject_train.txt, subject_test.txt, activity_labels.txt) into different variables. stringsAsFactors = FALSE is necessary to be able to join the feature vector with other character variables to form a vector of names of one of the intermediary results.

```{r}
features <- read.table("features.txt", stringsAsFactors = FALSE)
feat_test <- read.table("test/X_test.txt") # measured features of the test set
act_test <- read.table("test/y_test.txt") # activity codes for each record in the test set
feat_train <- read.table("train/X_train.txt") # measured features of the training set
act_train <- read.table("train/y_train.txt") # activity codes for each record in the training set
subject_train <- read.table("train/subject_train.txt") # subject codes for each record in the training set
subject_test <- read.table("test/subject_test.txt") # subject codes for each record in the test set
act_labels <- read.table("activity_labels.txt") # activity codes and their meanings
```

Then we separately merge the recorded activities codes with their meanings so that we have an activity name for each record. The variable that should match is "V1" in all cases (name assigned to the first column by default).
```{r}
actnames_test <- merge(act_test, act_labels, by.x = "V1", by.y = "V1")
actnames_train <- merge(act_train, act_labels, by.x = "V1", by.y = "V1")
```

Then we join (using bind_cols) the feature measurements, the activity names (second column in  the actnames_test and actnames_train data frames) and the subject codes for each data record into one data frame, separately for the training and the test datasets.

```{r}
join_test <- bind_cols(as.data.frame(actnames_test$V2), as.data.frame(subject_test$V1), feat_test)
join_train <- bind_cols(as.data.frame(actnames_train$V2), as.data.frame(subject_train$V1), feat_train)
```

Then we assign the names to the both resulting train and test datasets. Accordning the order of the variables in the function bind_cols (see above), the first two names should be "activity" and "subject". The rest of the names are contained in the second column of the features data frame:

```{r}
names(join_test) <- c("activity", "subject", features$V2)
names(join_train) <- c("activity", "subject", features$V2)
```

Then we finally join the training and the test datasets into the join dataset by binding the rows:

```{r}
join <- bind_rows(join_test, join_train)
```

Using the regular expression with the grep command we select the column names that contain character sequences "mean(" or "std(" while not forgetting the first two columns (activity and subject). Note that there are other features in the dataset that contain "mean" as part of their name (e.g. fBodyGyro-meanFreq()-X) which we are not interesting in. Therefore we look for "mean(" in the names of the columns of the join dataset.

```{r}
sel_join <- join[,c(1, 2, grep(".*mean\\(.*|.*std\\(.*", names(join)))]
```

To be able to smoothly separate the variables in the next steps we need to transform hte names of the variables. We see that they contain several hyphens, which can turn out to be a problem when separating the character values. First we delete all hyphens from the variable names:

```{r}
names(sel_join) <- gsub("-", "", names(sel_join))
```

Then we look for the expression "mean" in the names of the variables, and if found move to the end of the name of the variable, at the same time we separate it by a underscore sign from the rest of the name. We repeate this procedure for the "std" expression.

```{r}
names(sel_join) <- gsub("(.*)(mean\\(\\))(.*)", "\\1\\3_mean", names(sel_join))
names(sel_join) <- gsub("(.*)(std\\(\\))(.*)", "\\1\\3_std", names(sel_join))
```

The data set contains multiple variables in one column. Every feature is either a mean or a standard deviation of a certain metric. The metric itself has to be separated from the statistical measure. In fact it is not obvious whether "mean and "std" or all the metrics (tBodyAcc-X, tBodyAcc-Y etc.) should be the new column names. We opt for the first variant.


Since a combination of activity and subject can repeat several times, we need an identifier of each record to be able to associate means and stds of a certain metric relating to a certain activity and certain subject with each other in the course of the following manipulations. We create a new variable "obs" to decode each of the record in the initial dataset.

```{r}
sel_join <- mutate(sel_join, obs = 1:nrow(sel_join))  
```
Next we "gather" all columns except activity, subject and obs (i.e. all feature data)  convert all of them into two columns: var (with values being all feature value) and value (with values being all respective measured feature values)

```{r}
sel_join_g <- gather(sel_join, var, value, -(activity:subject), -obs)
```
Next we separate the var column into two columns "feature" and "metric". We have just one non-alphanumerical character in the var column (underscore), and it is automatically recognised as a separator. "feature" column now contains the feature names without suffixes "mean" or "std", and the column "metric" contains values "mean" and "std":

```{r}
sel_join_sep <- separate(sel_join_g, var, c("feature", "metric"))
```

Next we "spread" the metric column and define its values "mean" and "std" as the new colum names. The values that these columns should contain are the values in the "value" column.

```{r}
sel_join_spr <- spread(sel_join_sep, metric, value)
```
At last we group our dataset by activity, subject and (naturally) by feature to be able to compute summary metric per group:

```{r}
grouped_dataset <- group_by(sel_join_spr, activity, subject, feature)
```

Our final result is the "sum_dataset" data frame which is the grouped dataset with two additional variables: avg_mean, which is the average mean value of the respective feature relating to a certain activity and a certain subject, and avg_std, which is the average standard deviation of the respective feature relating to a certain activity and a certain subject. We convert the sun_dataset into tbl format that optimises the visualisation of the dataset.

```{r}
sum_dataset <- mutate(grouped_dataset, avg_mean = mean(mean), avg_std = mean(std))
sum_dataset <- tbl_df(sum_dataset)
```

The last step is to write the dataset into the file:


```{r}
write.table(sum_dataset, file = "final_dataset.txt", row.name = FALSE)
```
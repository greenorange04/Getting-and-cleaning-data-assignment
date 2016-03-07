library(dplyr) 
library(tidyr)
features <- read.table("features.txt", stringsAsFactors = FALSE)
feat_test <- read.table("test/X_test.txt")
act_test <- read.table("test/y_test.txt")
feat_train <- read.table("train/X_train.txt")
act_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
act_labels <- read.table("activity_labels.txt")

actnames_test <- merge(act_test, act_labels, by.x = "V1", by.y = "V1")
actnames_train <- merge(act_train, act_labels, by.x = "V1", by.y = "V1")

join_test <- bind_cols(as.data.frame(actnames_test$V2), as.data.frame(subject_test$V1), feat_test)
join_train <- bind_cols(as.data.frame(actnames_train$V2), as.data.frame(subject_train$V1), feat_train)
names(join_test) <- c("activity", "subject", features$V2)
names(join_train) <- c("activity", "subject", features$V2)
join <- bind_rows(join_test, join_train)


sel_join <- join[,c(1, 2, grep(".*mean\\(.*|.*std\\(.*", names(join)))]
names(sel_join) <- gsub("-", "", names(sel_join))
names(sel_join) <- gsub("(.*)(mean\\(\\))(.*)", "\\1\\3_mean", names(sel_join))
names(sel_join) <- gsub("(.*)(std\\(\\))(.*)", "\\1\\3_std", names(sel_join))
sel_join <- mutate(sel_join, obs = 1:nrow(sel_join))  
sel_join_g <- gather(sel_join, var, value, -(activity:subject), -obs)
sel_join_sep <- separate(sel_join_g, var, c("feature", "metric"))
sel_join_spr <- spread(sel_join_sep, metric, value)

grouped_dataset <- group_by(sel_join_spr, activity, subject, feature)
sum_dataset <- mutate(grouped_dataset, avg_mean = mean(mean), avg_std = mean(std))

write.table(sum_dataset, file = "final_dataset.txt", row.name = FALSE)
library(data.table)

run_analysis <- function() {

	test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
	train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
	features <- read.table("./UCI HAR Dataset/features.txt")	
	
	features <- as.character(features[,2])
	feature_index <- grep("-mean()|-std()", features)	

	data <- rbind(test_data, train_data)
	names(data) <- features
	data <- data[,feature_index]	

	activities_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
	activities_train <- activities_train[[1]]
	activities_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
	activities_test <- activities_test[[1]]	

	for (i in 1:length(activities_train)) {
		if (activities_train[i] == 1) {
			activities_train[i] <- "Walking"
		}
		else if (activities_train[i] == 2) {
			activities_train[i] <- "Walking Upstairs"
		}
		else if (activities_train[i] == 3) {
			activities_train[i] <- "Walking Downstairs"
		}
		else if (activities_train[i] == 4) {
			activities_train[i] <- "Sitting"
		}
		else if (activities_train[i] == 5) {
			activities_train[i] <- "Standing"
		}
		else if (activities_train[i] == 6) {
			activities_train[i] <- "Laying"
		}
	}	

	for (i in 1:length(activities_test)) {
		if (activities_test[i] == 1) {
			activities_test[i] <- "Walking"
		}
		else if (activities_test[i] == 2) {
			activities_test[i] <- "Walking Upstairs"
		}
		else if (activities_test[i] == 3) {
			activities_test[i] <- "Walking Downstairs"
		}
		else if (activities_test[i] == 4) {
			activities_test[i] <- "Sitting"
		}
		else if (activities_test[i] == 5) {
			activities_test[i] <- "Standing"
		}
		else if (activities_test[i] == 6) {
			activities_test[i] <- "Laying"
		}
	}	

	activity <- c(activities_test, activities_train)
	data <- cbind(activity, data)
	write.table(data, "tidy_data.txt", row.name=FALSE)

	subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")[[1]]
	subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")[[1]]
	subject <- c(subject_test, subject_train)

	data <- cbind(subject, data)

	dsplit <- split(data, subject)
	data2 <- data.frame()

	for (i in 1:length(dsplit)) {
		data2[i] <- apply(dsplit[[i]], 2, mean)
	}

	data2
}

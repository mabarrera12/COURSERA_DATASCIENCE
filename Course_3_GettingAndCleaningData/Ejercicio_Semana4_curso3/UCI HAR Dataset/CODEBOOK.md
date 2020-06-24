# CODEBOOK 

## **Human Activity Recognition Using Smartphones Data Set** 

### The Data 

In the next link, all the information about the dataset can be found: 

[Dataset REPO] (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones) 

The data in the `UCI HAR Dataset` consisted of 4 `.txt` files:

1. activity_labels.txt
2. features.txt
3. features_info.txt
4. README.txt 

And 2 folders _test_ and _train_ each containing 3 `.txt` files such as:

1. `subject.txt` where the id of the subject was displayed.
2. `X.txt` all the meassured variables 
3. `Y.txt` id of the six activities carried out in order by each subject.

### The code 

In order, according to the code,  these are the transformations made:

	* The `data.table` and `reshape2` packages are installed. 
	* `x_test` and `y_train` files are read into data.table format
	* The feature labels are assigned to the x_test data and the activity labels are assigned to the y_test data.
	* The columns that measures on the mean and standard deviation where extracted
	* A table (data_test) was created by column binding the x_test data, y_test data and the subject information.

The same steps where used to create a data_train table containing the information in the train folder.

	* both tables (`data_test` and `data_train`) where rowbinded and the average of each variable, activity and subject was calculated.
	* Finally a `Cleaned_data.txt` is created in the working directory.
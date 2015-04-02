#' Function for classification of patients by chosen category via discriminant analysis
#'
#' This function allows you to test classification of patients by some chosen category via discriminant analysis. 
#' @param data Accepts a list with first row containing adult data and second row containing child data.
#' @param variable Accepts a variable upon which to attempt to classify the data
#' @param modelType Accepts the type of model to use. Options are MclustDA, which models the classes as a a mixture model of multiple normal distributions, or EDDA, which models each class as a single normal distribution. Defaults to MclustDA.
#' @param modelNames Accepts the name of the matrix constraint on the covariance matrix. See Mclust documentation for names of of constraints. Defaults to NULL.
#' @keywords Discriminant analysis
#' @export
#' @examples
#' DA()
DA <- function(data, variable, modelType = "MclustDA", modelNames = NULL){
	require(mclust)

	#Get data
	data_adults <- data[[1]]
	data_children <- data[[2]]

	#Find which columns correspond to the given variable
	var_col <- which(colnames(data_adults)== variable)
	if(length(var_col) == 0){
		print('Entered variable does not exist in dataset')
		return(NULL)
	}
	
	#Isolate columns of interest
	data_adults <- data_adults[,c(var_col,381:636)]
	data_children <- data_children[,c(var_col,381:636)]

	#Remove NAs
	data_adults <- na.omit(data_adults);
	data_children<-na.omit(data_children);

	#Separate data
	data_adults_baseline <- data_adults[,c(1:129)];
	data_adults_concentration <- data_adults[,c(1,130:256)];
	data_adults_combined <- data_adults[,c(1:257)];
	data_children_baseline <- data_children[,c(1:129)];
	data_children_concentration <- data_children[,c(1,130:256)];
	data_children_combined <- data_children[,c(1:257)];

	#If gender is the variable, it is necessary to remove unknowns
	if (var_col == 8){
		#Remove "Unknown" from the gender column
		data_adults_baseline <- as.data.frame(data_adults_baseline[!(data_adults_baseline[1] == "Unknown"),]);
		data_adults_concentration <- as.data.frame(data_adults_concentration[!(data_adults_concentration[1] == "Unknown"),]);
		data_adults_combined <- as.data.frame(data_adults_combined[!(data_adults_combined[1] == "Unknown"),]);
		data_children_baseline <- as.data.frame(data_children_baseline[!(data_children_baseline[1] == "Unknown"),]);
		data_children_concentration <- as.data.frame(data_children_concentration[!(data_children_concentration[1] == "Unknown"),]);
		data_children_combined <- as.data.frame(data_children_combined[!(data_children_combined[1] == "Unknown"),]);

		#Remove "Unknown" as a possible gender level
		data_adults_baseline <- droplevels(data_adults_baseline);
		data_adults_concentration <- droplevels(data_adults_concentration);
		data_adults_combined <- droplevels(data_adults_combined);
		data_children_baseline <- droplevels(data_children_baseline);
		data_children_concentration <- droplevels(data_children_concentration);
		data_children_combined <- droplevels(data_children_combined);
	}
	
	#Adults Baseline%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	results_1 <- MclustDA(data_adults_baseline[,-1],data_adults_baseline[,1], modelType =  modelType, modelNames = modelNames)
	error_1 <- cv.MclustDA(results_1, nfold = 10, verbose = TRUE) 
	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	#Repeat the above operations for all following data sets

	#Adults Concentration%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	results_2 <- MclustDA(data_adults_concentration[,-1],data_adults_concentration[,1], modelType =  modelType, modelNames = modelNames)
	error_2 <- cv.MclustDA(results_2, nfold = 10, verbose = TRUE) 
	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	#Adults Combined%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	results_3 <- MclustDA(data_adults_combined[,-1],data_adults_combined[,1], modelType =  modelType, modelNames = modelNames)
	error_3 <- cv.MclustDA(results_3, nfold = 10, verbose = TRUE) 
	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	#Children Baseline%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	results_4 <- MclustDA(data_children_baseline[,-1],data_children_baseline[,1], modelType =  modelType, modelNames = modelNames)
	error_4 <- cv.MclustDA(results_4, nfold = 10, verbose = TRUE) 
	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	#Children Concentration%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	results_5 <- MclustDA(data_children_concentration[,-1],data_children_concentration[,1], modelType =  modelType, modelNames = modelNames)
	error_5 <- cv.MclustDA(results_5, nfold = 10, verbose = TRUE) 
	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	#Children Combined%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	results_6 <- MclustDA(data_children_combined[,-1],data_children_combined[,1], modelType =  modelType, modelNames = modelNames)
	error_6 <- cv.MclustDA(results_6, nfold = 10, verbose = TRUE) 
	#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	# results <- list(summary(results_1),as.character(error_1$error),summary(results_2), as.character(error_2$error),summary(results_3),as.character(error_3$error),summary(results_4),as.character(error_4$error),summary(results_5),as.character(error_5$error),summary(results_6),as.character(error_6$error))
	# return(capture.output(results))
	average_error <- 1 - mean(c(error_1$error,error_2$error,error_3$error,error_4$error,error_5$error,error_6$error))
	result <- paste('The average error is ',as.character(average_error))
}
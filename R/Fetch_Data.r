#' Function to fetch data
#'
#' This function fetches data contained in the Amen SPECT image datasets.
#' @param set_number Specify whether to fetch the training set(1) or test set(2). Defaults to 1.
#' @keywords Fetch data
#' @export
#' @examples
#' Fetch_Data()

#Take in a parameter specifying to either load the training set (1) or testing set (2). Throw an error for any other parameter. 
#Return a list containing 2 elements: 1)An Adult data frame; 2) A child data frame.

Fetch_Data <- function(set_number = 1){
	require(foreign)
	require(bigmemory)
	
	#Default to loading the training set
	if (set_number == 1){
		Adults = file.path(dirname(getwd()),"Data","Base","Data_Set_1","Data_Adults_1.csv")
		Children = file.path(dirname(getwd()),"Data","Base","Data_Set_1","Data_Children_1.csv")

		data_adults <- read.csv(Adults,nrows = 8000);
		data_children <- read.csv(Children,nrows = 3000);
		
		output <- list(data_adults,data_children)
		
		return(output)
	} 
	
	#User may also load the testing set
	else if(set_number == 2){
		Adults = file.path(dirname(getwd()),"Data","Base","Data_Set_2","Data_Adults_2.csv")
		Children = file.path(dirname(getwd()),"Data","Base","Data_Set_2","Data_Children_2.csv")

		data_adults <- read.csv(Adults,nrows = 8000);
		data_children <- read.csv(Children,nrows = 3000);
		
		output <- list(data_adults,data_children)
		
		return(output)
	} 
	
	#Throw an error for any other input parameter
	else{
		stop('Not a valid data set index number!')
	}
}
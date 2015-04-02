#' Function for generating mean vector of SPECT image brightness values in different regions of the brain
#'
#' This function generates and plots a vector containing the mean SPECT image brightness value sin different regions of the brain.
#' @param data Accepts a list with first row containing adult data and second row containing child data.
#' @keywords Mean
#' @export
#' @examples
#' Mean_Vectors()

Mean_Vectors <- function(data){
	require(graphics)
	require(ggplot2)
	require(reshape)
	
	#Split data
	data_adults <- data[[1]]
	data_children <-data[[2]]
	
	#Isolate columns of interest
	data_adults <- data_adults[,c(381:636)]
	data_children <- data_children[,c(381:636)]

	#Separate data
	data_adults_baseline <- data_adults[,c(1:128)];
	data_adults_concentration <- data_adults[,c(129:256)];
	data_children_baseline <- data_children[,c(1:128)];
	data_children_concentration <- data_children[,c(129:256)];

	#Find mean vectors
	mean_adults_baseline = colMeans(data_adults_baseline, na.rm = TRUE)
	mean_adults_concentration = colMeans(data_adults_concentration, na.rm = TRUE)
	mean_children_baseline = colMeans(data_children_baseline, na.rm = TRUE)
	mean_children_concentration = colMeans(data_children_concentration, na.rm = TRUE)

	#Find standard errors
	se_mean_adults_baseline = apply(data_adults_baseline, 2, sd,  na.rm = TRUE) / sqrt(dim(data_adults_baseline)[2]);
	se_mean_adults_concentration = apply(data_adults_baseline, 2, sd,  na.rm = TRUE) / sqrt(dim(data_adults_baseline)[2]);
	se_mean_children_baseline = apply(data_adults_baseline, 2, sd,  na.rm = TRUE) / sqrt(dim(data_adults_baseline)[2]);
	se_mean_children_concentration = apply(data_adults_baseline, 2, sd,  na.rm = TRUE) / sqrt(dim(data_adults_baseline)[2]);


	#Convert to data frame
	df_mean_adults_baseline = data.frame(x = c(1:length(mean_adults_baseline)), y = mean_adults_baseline, ymax = mean_adults_baseline + se_mean_adults_baseline, 
		ymin = mean_adults_baseline - se_mean_adults_baseline)
	df_mean_adults_concentration = data.frame(x = c(1:length(mean_adults_concentration)), y = mean_adults_concentration, ymax = mean_adults_concentration + se_mean_adults_concentration, 		ymin = mean_adults_concentration - se_mean_adults_concentration)
	df_mean_children_baseline = data.frame(x = c(1:length(mean_children_baseline)), y = mean_children_baseline, ymax = mean_children_baseline + se_mean_children_baseline, ymin = mean_children_baseline - se_mean_children_baseline)
	df_mean_children_concentration = data.frame(x = c(1:length(mean_children_concentration)), y = mean_children_concentration, ymax = mean_children_concentration + se_mean_children_concentration, ymin = mean_children_concentration - se_mean_children_concentration)

	#Plot vectors
	print(ggplot(df_mean_adults_baseline,aes(x,y)) + geom_point() + geom_errorbar(aes(ymax = ymax,ymin = ymin)) + geom_line() + xlab('Category Number') + ylab('Mean') + ggtitle('Adult Baseline Mean Vector'))
	
	print(ggplot(df_mean_adults_concentration,aes(x,y)) + geom_point() + geom_errorbar(aes(ymax = ymax,ymin = ymin)) + geom_line() + xlab('Category Number') + ylab('Mean') + ggtitle('Adult Concentration Mean Vector'))
	
	print(ggplot(df_mean_children_baseline,aes(x,y)) + geom_point() + geom_errorbar(aes(ymax = ymax,ymin = ymin)) + geom_line() + xlab('Category Number') + ylab('Mean') + ggtitle('Children Baseline Mean Vector'))
	
	print(ggplot(df_mean_children_concentration,aes(x,y)) + geom_point() + geom_errorbar(aes(ymax = ymax,ymin = ymin)) + geom_line() + xlab('Category Number') + ylab('Mean') + ggtitle('Children Concentration Mean Vector'))
}


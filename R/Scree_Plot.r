#' Function for generating Scree Plots of Amen Data
#'
#' This function generates Scree Plots of Amen Data.
#' @param data Accepts a list with first row containing adult data and second row containing child data.
#' @keywords Scree, Variance
#' @export
#' @examples
#' Scree_Plot

Scree_Plot <- function(data){
	require(ggplot2)
	require(graphics)

	#Get data
	data_adults <- data[[1]]
	data_adults <- data[[2]]

	#Isolate columns of interest
	data_adults <- data_adults[,c(381:636)]
	data_children <- data_children[,c(381:636)]

	#Remove NAs
	data_adults <- na.omit(data_adults);
	data_children<-na.omit(data_children);

	#Separate data
	data_adults_baseline <- t(data_adults[,c(1:128)]);
	data_adults_concentration <- t(data_adults[,c(129:256)]);
	data_adults_combined <- t(data_adults[,c(1:256)]);
	data_children_baseline <- t(data_children[,c(1:128)]);
	data_children_concentration <- t(data_children[,c(129:256)]);
	data_children_combined <- t(data_children[,c(1:256)]);

	#Generate scree data, plot it, and output it to file for all data sets.
	fit <- prcomp(data_adults_baseline,center = TRUE,scale = TRUE);
	results <- data.frame(x = c(1:length(unlist(fit[1]))), y = cumsum(unlist(fit[1])^2) /  max(cumsum(unlist(fit[1])^2)));
	print(ggplot(results,aes(x,y)) + geom_line() + geom_point() + xlab("Components") + ylab("Percent Variance") + ggtitle("Adult Baseline Scree Plot"))

	fit <- prcomp(data_adults_concentration,center = TRUE,scale = TRUE);
	results <- data.frame(x = c(1:length(unlist(fit[1]))), y = cumsum(unlist(fit[1])^2) /  max(cumsum(unlist(fit[1])^2)));
	print(ggplot(results,aes(x,y)) + geom_line() + geom_point() + xlab("Components") + ylab("Percent Variance") + ggtitle("Adults Concentration Scree Plot"))


	fit <- prcomp(data_children_baseline,center = TRUE,scale = TRUE);
	results <- data.frame(x = c(1:length(unlist(fit[1]))), y = cumsum(unlist(fit[1])^2) /  max(cumsum(unlist(fit[1])^2)));
	print(ggplot(results,aes(x,y)) + geom_line() + geom_point() + xlab("Components") + ylab("Percent Variance") + ggtitle("Children Baseline Scree Plot"))

	fit <- prcomp(data_children_concentration,center = TRUE,scale = TRUE);
	results <- data.frame(x = c(1:length(unlist(fit[1]))), y = cumsum(unlist(fit[1])^2) /  max(cumsum(unlist(fit[1])^2)));
	print(ggplot(results,aes(x,y)) + geom_line() + geom_point()+ xlab("Components") + ylab("Percent Variance") + ggtitle("Children Concentration Scree Plot"))

	fit <- prcomp(data_adults_combined,center = TRUE,scale = TRUE);
	results <- data.frame(x = c(1:length(unlist(fit[1]))), y = cumsum(unlist(fit[1])^2) /  max(cumsum(unlist(fit[1])^2)));
	print(ggplot(results,aes(x,y)) + geom_line() + geom_point()+ xlab("Components") + ylab("Percent Variance") + ggtitle("Adult Combined Scree Plot"))

	fit <- prcomp(data_children_combined,center = TRUE,scale = TRUE);
	results <- data.frame(x = c(1:length(unlist(fit[1]))), y = cumsum(unlist(fit[1])^2) /  max(cumsum(unlist(fit[1])^2)));
	print(ggplot(results,aes(x,y)) + geom_line() + geom_point()+ xlab("Components") + ylab("Percent Variance") + ggtitle("Children Combined Scree Plot"))
}



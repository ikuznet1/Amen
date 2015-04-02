
#' Marginal Distribution Function
#'
#' Generates the marginal distribution of SPECT image brightness values in different regions of the brain
#' @param data Accepts a list with first row containing adult data and second row containing child data.
#' @param print_adults Determines whether histograms for adults are printed. Defaults to TRUE.
#' @param print_children Determines whether histograms for children are printed. Defaults to TRUE.
#' @keywords Create marginal distributions.
#' @export
#' @examples
#' Hist_1D()
#Function that generates the null distributions of the Adult and/or Children data. It takes as input the data, a boolean indicating whether to generates the adult distributions and a boolean indicating whether to generate the children distributions.

Hist_1D <- function(data, print_adults = TRUE, print_children = TRUE){
	#Load libraries
	require(ggplot2)
	require(reshape)

	#Plot histograms with kernel density estimation and associated data points with jitter (for adults and children). Use subplots with graph for left of brain on left and right of brain on right.
	
	if (print_adults){
		#Consider only adult data
		data_adults <- data[[1]]
		
		#Isolate columns of interest
		data_adults <- data_adults[,c(8,381:636)]
		
		#Separate into left and right brain hemispheres
		data_adults_L <- data_adults[,c(1,1:128 * 2)]
		data_adults_R  <- data_adults[,c(1,1:128 * 2 + 1)]
		
		#Remove NAs
		data_adults_L <- na.omit(data_adults_L);
		data_adults_R <- na.omit(data_adults_R);	
		
		#Iterate through and plot
		for (i in c(2:129)) {
			subset<- data_adults_L[1:dim(data_adults_L)[2],i]
			df <- data.frame(Gender = data_adults_L[1:dim(data_adults_L)[2],1], values = subset, height = abs(rnorm(dim(data_adults_L)[2], 0 , 0.00025)))
			p1 <- ggplot(df,aes(x=values, colour = Gender)) + geom_point(aes(y = height),alpha = 0.5) + geom_density() + xlab("Value") + ylab("Frequency") + ggtitle(colnames(data_adults_L)[i]) + theme(legend.position="bottom")
			subset<- data_adults_R[1:dim(data_adults_R)[2],i]
			df <- data.frame(Gender = data_adults_R[1:dim(data_adults_R)[2],1], values = subset, height = abs(rnorm(dim(data_adults_R)[2], 0 , 0.00025)))
			p2 <- ggplot(df,aes(x=values, colour = Gender)) + geom_point(aes(y = height),alpha = 0.5) + geom_density() + xlab("Value") + ylab("Frequency") + ggtitle(colnames(data_adults_R)[i]) + theme(legend.position="bottom")
			Multiplot(p1,p2,cols = 2)
		}
	}

	if (print_children){
		#Consider only child data
		data_children <- data[[2]]

		#Isolate columns of interest
		data_children <- data_children[,c(8,381:636)]

		#Separate into left and right brain hemispheres
		data_children_L <- data_children[,c(1,1:128 * 2)]
		data_children_R  <- data_children[,c(1,1:128 * 2 + 1)]

		#Remove NAs
		data_children_L<-na.omit(data_children_L);
		data_children_R<-na.omit(data_children_R);
		
		#Iterate through and plot
		for (i in c(2:129)) {
			subset<- data_children_L[1:dim(data_children_L)[2],i]
			df <- data.frame(Gender = data_children_L[1:dim(data_children_L)[2],1], values = subset, height = abs(rnorm(dim(data_children_L)[2], 0 , 0.00025)))
			p1 <- ggplot(df,aes(x=values, colour = Gender)) + geom_point(aes(y = height),alpha = 0.5) + geom_density() + xlab("Value") + ylab("Frequency") + ggtitle(colnames(data_children_L)[i]) + theme(legend.position="bottom")
			subset<- data_children_R[1:dim(data_children_R)[2],i]
			df <- data.frame(Gender = data_children_R[1:dim(data_children_R)[2],1], values = subset, height = abs(rnorm(dim(data_children_R)[2], 0 , 0.00025)))
			p2 <- ggplot(df,aes(x=values, colour = Gender)) + geom_point(aes(y = height),alpha = 0.5) + geom_density() + xlab("Value") + ylab("Frequency") + ggtitle(colnames(data_children_R)[i]) + theme(legend.position="bottom")
			Multiplot(p1,p2,cols = 2)
		}
	}
}



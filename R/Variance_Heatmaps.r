#' Function for generated heatmap of the covariance matrix of the Amen data
#'
#' This function generates a heatmap of the covariance matrix of the Amen data
#' @param data Accepts a list with first row containing adult data and second row containing child data.
#' @export
#' @examples
#' Variance-Heatmaps()

Variance_Heatmaps <- function(data){
	require(graphics)
	require(ggplot2)
	require(reshape)
	
	#Split data
	data_adults <- data[[1]]
	data_children <-data[[2]]
		
	#Find columns of interest 
	data_adults <- data_adults[,c(8,381:636)]
	data_children <- data_children[,c(8,381:636)]

	data_adults_baseline <- data_adults[,c(1,2:129)];
	data_adults_concentration <- data_adults[,c(1,130:257)];
	data_children_baseline <- data_children[,c(1,2:129)];
	data_children_concentration <- data_children[,c(1,130:257)];

	#Separate into male and female
	data_adults_baseline_m <- data_adults_baseline[grep('Male', data_adults_baseline$Gendername),]
	data_adults_baseline_f <- data_adults_baseline[grep('Female', data_adults_baseline$Gendername),]
	data_adults_concentration_m <- data_adults_baseline[grep('Male', data_adults_concentration$Gendername),]
	data_adults_concentration_f <- data_adults_baseline[grep('Female', data_adults_concentration$Gendername),]
	data_children_baseline_m <- data_children_baseline[grep('Male', data_children_baseline$Gendername),]
	data_children_baseline_f <- data_children_baseline[grep('Female', data_children_baseline$Gendername),]
	data_children_concentration_m <- data_children_concentration[grep('Male', data_children_concentration$Gendername),]
	data_children_concentration_f <- data_children_concentration[grep('Female', data_children_concentration$Gendername),]


	#Find variance vectors 
	covariance_adults_baseline_m = var(data_adults_baseline_m[,2:129], na.rm = TRUE)
	covariance_adults_baseline_f = var(data_adults_baseline_f[,2:129], na.rm = TRUE)
	covariance_adults_concentration_m = var(data_adults_concentration_m[,2:129], na.rm = TRUE)
	covariance_adults_concentration_f = var(data_adults_concentration_f[,2:129], na.rm = TRUE)
	covariance_children_baseline_m = var(data_children_baseline_m[,2:129], na.rm = TRUE)
	covariance_children_baseline_f = var(data_children_baseline_f[,2:129], na.rm = TRUE)
	covariance_children_concentration_m = var(data_children_concentration_m[,2:129], na.rm = TRUE)
	covariance_children_concentration_f = var(data_children_concentration_f[,2:129], na.rm = TRUE)

	#Plot results with formatting
	name <- covariance_adults_baseline_m
	name <- melt(name)
	plot_results <- ggplot(name, aes(X1, X2)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
	base_size <- 14
	print(plot_results + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = base_size)) + ggtitle("Male Adults Baseline Covariance Heatmap"))

	name <- covariance_adults_baseline_f
	name <- melt(name)
	plot_results <- ggplot(name, aes(X1, X2)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
	print(plot_results + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = base_size)) + ggtitle("Female Adults Baseline Covariance Heatmap"))

	name <- covariance_adults_concentration_m
	name <- melt(name)
	plot_results <- ggplot(name, aes(X1, X2)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
	print(plot_results + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = base_size)) + ggtitle("Male Adults Concentration Covariance Heatmap"))

	name <- covariance_adults_concentration_f
	name <- melt(name)
	plot_results <- ggplot(name, aes(X1, X2)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
	print(plot_results + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = base_size)) + ggtitle("Female Adults Concentration Covariance Heatmap"))

	name <- covariance_children_baseline_m
	name <- melt(name)
	plot_results <- ggplot(name, aes(X1, X2)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
	print(plot_results + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = base_size)) + ggtitle("Male Children Baseline Covariance Heatmap"))

	name <- covariance_children_baseline_f
	name <- melt(name)
	plot_results <- ggplot(name, aes(X1, X2)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
	print(plot_results + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = base_size)) + ggtitle("Female Children Baseline Covariance Heatmap"))

	name <- covariance_children_concentration_m
	name <- melt(name)
	plot_results <- ggplot(name, aes(X1, X2)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
	print(plot_results + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = base_size)) + ggtitle("Male Children Concentration Covariance Heatmap"))
	
	name <- covariance_children_concentration_f
	name <- melt(name)
	plot_results <- ggplot(name, aes(X1, X2)) + geom_tile(aes(fill = value),colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
	print(plot_results + theme_grey(base_size = base_size) + labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(axis.ticks = element_blank(), axis.text.x = element_blank(),axis.text.y = element_blank(), legend.text = element_text(size = base_size)) + ggtitle("Female Children Concentration Covariance Heatmap"))
}









#######################
# ALDA: hw0.R
# Instructor: Dr. Thomas Price
# Mention your team details here
# Saad Mohammad Abrar, sabrar
# Nasif Imtiaz, simtiaz
# Taufiq Islam Protick, tprotic
# 
#########################
require(ggplot2)
set.seed(321)
# no install.packages or rm(list=ls(all=T)) in this function


intro_to_r <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: a  list:  [generated vector (type: double), mean of vector (type: double), median of the vector (type: double), max value of vector (type: double), min value of the vector (type: double)]
  # The elements in `new_vector` are random numbers that follow a normal distribution (mean = 0, standard deviation = 1)
  new_vector <- rnorm(num_values, mean = 0, sd = 1)
  new_mean <- mean(new_vector)
  new_median <- median(new_vector)
  new_max <- max(new_vector)
  new_min <- min(new_vector)
  # Returning the vector, and the mean, median, maximum, and minimum of the vectors in a list form 
  return(list(new_vector, new_mean, new_median, new_max, new_min))
}

intro_to_plotting <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: two plots (saved to disk, no return value), descriptions for which have been provided in the hw0 document.
  # The elements in `new_vector` are random numbers that follow a normal distribution (mean = 0, standard deviation = 1)
  new_vector <- rnorm(num_values, mean = 0, sd = 1)
  # `data` param in the parameters of ggplot() must be a data frame, so converting `new_vector' to a data frame `df`
  df <- as.data.frame(new_vector)
  
  # Scatter plot with `new_vector` on x-axis and `new_vector` on y-axis and saving the plot programmatically
  ggplot(df, aes(x = new_vector, y = new_vector))+geom_point()
  ggsave("G14_plot01.pdf")
  # Scatter plot with `new_vector` on x-axis and `new_vector^2` on y-axis and saving the plot programmatically
  ggplot(df, aes(x = new_vector, y = new_vector^2))+geom_point()
  ggsave("G14_plot02.pdf")
}

# do not call either function in this script 
# this script is only for function definitions
# if you wish to test your script, call your functions separately in a separate script.
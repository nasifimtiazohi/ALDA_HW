#######################
# ALDA: hw0.R
# Instructor: Dr. Thomas Price
# Mention your team details here
#
#
#
#
#########################
require(ggplot2)
set.seed(123)
# no install.packages or rm(list=ls(all=T)) in this function


intro_to_r <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: a  list:  [generated vector (type: double), mean of vector (type: double), median of the vector (type: double), max value of vector (type: double), min value of the vector (type: double)]
  new_vector<-rnorm(num_values)
  #new_vector
  new_mean <- mean(new_vector)
  new_median <- median(new_vector)
  new_max <-max(new_vector)
  new_min <- min(new_vector)
  
  ret_list<-c(new_mean,new_median,new_max,new_min)
  ret_list
}

intro_to_plotting <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: two plots (saved to disk, no return value), descriptions for which have been provided in the hw0 document. 
  new_vector<-rnorm(num_values)
  new_vector2<-new_vector
  mat <- cbind(new_vector,new_vector2)
  values <- as.data.frame(mat)
  ggplot(values,aes(new_vector,new_vector2))+geom_point()
  ggsave("G14_plot01.png")
  
  new_vector2<-new_vector^2
  mat2 <- cbind(new_vector,new_vector2)
  values2 <- as.data.frame(mat2)
  ggplot(values2,aes(new_vector,new_vector2))+geom_point()
  ggsave("G14_plot02.png")
}

# do not call either function in this script 
# this script is only for function definitions
# if you wish to test your script, call your functions separately in a separate script.
###########
# HW1
# Mention your team details here
# Saad Mohammad Abrar, sabrar
# Taufiq Islam Protick, tprotic
# Nasif Imtiaz, simtiaz
############


# You may use the following librarie(s):
require(plyr)
# If you get an error when running these lines, 
# make sure to install the respective libraries

# read data matrix
read_data <- function(path = "./iris.csv") {
  # Note 1: DO NOT change the function arguments.
  # Input: path: type: string, output: a matrix containing data from iris.csv
  # Write code here to read the csv file as a data frame and return it.
  data <- read.csv(path)
  return(data)
}

# Part 1: Distance Measurement
calculate_euclidean <- function(p, q) {
  # Input: p, q are numeric vectors of the same length
  # output: a single value of type double, containing the euclidean distance between p and q.
  # Converting p and q to numeric vectors (just vectors with double values).
  p <- as.numeric(p)
  q <- as.numeric(q)
  sum <- 0
  # Given that the inputs will be two vectors of same length, so iteration along the length of any one will do
  for (i in 1:length(p)){
    sum <- sum + ((p[i] - q[i])^2)
  }
  return(sqrt(sum))
}

calculate_cosine <- function(p, q) {
  # Input: p, q are numeric vectors of the same length
  # output: a single value of type double, containing the cosine distance between p and q.
  # Converting p and q to numeric vectors (just vectors with double values).
  p <- as.numeric(p)
  q <- as.numeric(q)
  modulus_p <- 0
  modulus_q <- 0
  # Calculating the modulus of p and q, i.e., ||p|| and ||q||
  for (i in 1:length(p)){
    modulus_p <- (modulus_p + p[i]^2)
    modulus_q <- (modulus_q + q[i]^2)
  }
  modulus_p <- sqrt(modulus_p)
  modulus_q <- sqrt(modulus_q)
  # Dot product of p and q
  dot_product <- 0
  for (i in 1:length(p)){
    dot_product <- (dot_product + p[i] * q[i])
  }
  # Return cosine dissimilarity
  return(1-(dot_product / (modulus_p * modulus_q)))
  
}

calculate_l_inf <- function(p, q) {
  # Input: p, q are numeric vectors of the same length
  # output: a single value of type double, containing the l_inf distance between p and q.
  # Converting p and q to numeric vectors (just vectors with double values).
  p <- as.numeric(p)
  q <- as.numeric(q)
  # Return L_inf according to the formula
  return(max(abs(p-q)))
}

# Part 2: principal Component Analysis
principal_component_analysis <- function(data, n){
  # Input: data: the Iris dataframe, with 4 numeric attributes and a 5th nominal class variable
  #        n: the number of the principle component to calculate (e.g. 1 for first principal component)
  # output: a 1 x 4 vector of type double, containing the weights (eigenvector) of the 
  # nth principal component of the dataset.

  # The final column in ``data`` is a nominal attribute that cannot be included in the calculation of PCA
  # Getting the data frame excluding the final column
  df <- data[,1:4]
  
  # calculate the pca using prcomp. prcomp considers computes PCs along the column attributes
  pca <- prcomp(df)

  # Among the value prcomp returns, rotation is the matrix whose columns contain the eigenvectors.
  # We need the nth PC and its eigenvector is in the nth column of pca$rotation.
  eigen_vector_PCn <- as.numeric(pca$rotation[,n])
  
  # Return the Eigen vector of the nth PC
  return(eigen_vector_PCn)
}

principal_component_calculation <- function(p, component_weights){
  # Input: p is a numeric vector of of length n, e.g. representing a row from the original dataset.
  #        component_weights is a vector length n, containing the weights of a principal component
  #        (e.g. the output from running principal_component_analysis)
  # Output: a single value of type double, containing the first principal component value of the sample.
  
  # principal component value of a sample p is the linear combination of p and the component_weights
  linear_combination <- p*component_weights
  return (sum(linear_combination))
}

pc1_distance <- function(p, q, component_weights) {
  # Input: p, q are numeric vectors of of length n, e.g. representing rows from the original dataset.
  #        component_weights is a vector length n, containing the weights of a principal component
  #        (e.g. the output from running principal_component_analysis)
  # output: a single value of type double, containing the distance between p and q, projected onto 
  # the first principal component (i.e. |PC1_p - PC1_q|)
  
  # Get the PC values.
  PC1_p <- principal_component_calculation(p, component_weights)
  PC1_q <- principal_component_calculation(q, component_weights)
  # Return the absolute difference of the two PC values
  return(abs(PC1_p - PC1_q))

}


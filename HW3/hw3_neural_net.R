###########################
# ALDA: hw3_neural_net.R 
# Instructor: Dr. Thomas Price
# Mention your team details here
# @author: Yang Shi/yangatrue
#
#
#
############################

# NOTE: In this homework, neural network will be calculated in matrix calculations rather than scalars.
# The input of the neural is a matrix of size (n,m), where n is the size of the training set, m is the number of attributes.
# All the calculations of derivatives, will therefore be matrix derivative calculations.


sigmoid <- function(x) {
  # Calculate sigmoid of a matrix x
  
  # Inputs:
  # x: a matrix of values
  x=exp(-x)
  x=1+x
  x=1/x
  # Output:
  # A matrix with the same size of the input x, where every element x_i is the result of sigmoid(x_i) 
  return(x)
}

sigmoid_derivative <- function(x) {
  # Calculate the derivative of sigmoid function with respect to a matrix x.
  
  # Inputs:
  # x: a matrix of values
  x=x*(1-x)
  # Output:
  # A matrix with the same size of the input x, where every element x_i is the result of the derivative of sigmoid(x_i).
  return(x)
}

calculate_loss <- function(y_pred, y) {
  # Calculate the loss of predictions and the label.
  
  # Inputs:
  # y_pred: a vector of activations from the last layer of the network.
  # y: a vector of the label of the training samples.
  se = sum((y - y_pred)^2)
  # Output:
  # A number that is the total MSE loss of y_pred and y.
  return(se)
}

calculate_activations <- function(input_matrix, weight_matrix) {
  # Calculate the activations of a layer
  
  # Inputs:
  # input_matrix: a matrix, composed of vectors of inputs. The size of the matrix is (n,m), 
  # where n is the number of samples, and m is the number of the attributes, 
  # or the number of hidden units from last layer.
  # weight_matrix: a matrix, containing the weight for a layer. The dimention of the matrix is (m,q),
  # where q is the number of hidden units for this layer.
  # Output:
  # A matrix with the size (n,q), activated by the sigmoid function. 
  output=sigmoid(input_matrix %*% weight_matrix)
  return(output)
}

calculate_dCdw <- function(in_activations, out_activations, out_dCdf) {

  # Calculate the derivative of loss function with respect to a weight matrix w
  
  # Inputs:
  # in_activations: a matrix of the original input of the layer with weight w.
  # out_activations: a matrix of the original output of the layer with the weight w.
  # out_dCdf: The derivative of the loss function to the out_activations.
  
  # Hint 1: in the case of the last layer, out_dCdf would be the derivative of the loss
  # with respect to the activation of the last layer, which is y_pred.
  
  # Hint 2: Remember that dC/dw = dC/df * df/dx * dx/dw, where:
  # C is the cost function, f is the activation's output, x is input to the activation function, and w is a weight
  # Use the derivatives you calcualted in Problem 3 of the homework to help you implement this function
  dfdx=sigmoid_derivative(out_activations)
  dCdx= out_dCdf * dfdx
  dCdw= t(in_activations) %*% dCdx
  # Output:
  # A matrix with the same size of the target matrx w, recording the derivative of loss to w.
  return(dCdw)  
}

# Note: 522 Only
calculate_dCdf <- function(weight_matrix, out_activations, out_dCdf) {
  # Calculate the derivative of loss function with respect to an activation output of one layer
  
  # Inputs:
  # weight_matrix: a weight matrix for the current layer
  # out_activations: a matrix of the activation values output from this layer.
  # out_dCdf: The derivative of the loss function to the out_activations of this layer.
  
  # Hint 1: This will only be needed in cases of a 2-layer network.
  # Hint 2: Remember that dC/df_{L-1} = dCdf_L/df_L * df_L/dx * dx/df_{L-1}, where:
  # C is the cost function, f_{L-1} is the activation at the previous layer, f_L is the activation at this layer,
  # and x is input to the activation function at this layer
  dCdf= (out_dCdf * sigmoid_derivative(out_activations)) %*% t(weight_matrix)
  # Output:
  # A matrix with the same size of the activation f, recording dC/df_{L-1}, the derivative of loss to 
  # f, the activations of the previous layer.
  return(dCdf)
}


neuralnet <- function(x_train, y_train, nodes_layer = 4, n_attributes = 8, learning_rate=0.001, epochs=150) {
  # Implement the neural network.
  
  # Inputs:
  # x_train: The training dataset. A dataframe that has n samples, m attributes.
  # y_train: The labels for training dataset. A dataframe that has n samples, 1 column with the class values.
  # nodes_layer: Integer. In cases of 2-layer neural network, the number of neurons for the first layer is defined here.
  # n_attributes: Integer. Number of attributes.
  # learning_rate: Float. Learning rate of of the neural network.
  # epochs: The number of iterations in training process.
  
  #-------------------------------------------------------------#
  # Data and matrix initialization
  x_train=data.matrix(x_train)
  y_train=data.matrix(y_train)
  #initializa weight matrix
  set.seed(155)
  firstLayerWeight=matrix(rnorm(n_attributes*nodes_layer,mean=0,sd=1),  n_attributes, nodes_layer)
  secondLayerWeight=matrix(rnorm(nodes_layer,mean=0,sd=1), nodes_layer, 1)
  
  #-------------------------------------------------------------#
  # Training process
  for (i in 1:epochs) {
    #-------------------------------------------------------------#
    # Forward Propagation
    firstLayerOutput=calculate_activations(x_train,firstLayerWeight)
    secondLayerOutput = calculate_activations(firstLayerOutput,secondLayerWeight)
    #-------------------------------------------------------------#
    # Calculating training loss
    loss=calculate_loss(secondLayerOutput,y_train)
    
    #-------------------------------------------------------------#
    # Derivative calculation
    out_dCdf= 2 * (secondLayerOutput - y_train)
    secondLayerGradient = calculate_dCdw(firstLayerOutput,secondLayerOutput,out_dCdf)
    firsrLayer_dCdf = calculate_dCdf(secondLayerWeight,secondLayerOutput,out_dCdf)
    firtLayerGradient = calculate_dCdw(x_train,firstLayerOutput,firsrLayer_dCdf)
    #-------------------------------------------------------------#
    # Updating weight matrices
    secondLayerWeight = secondLayerWeight + learning_rate * secondLayerGradient
    firstLayerWeight =  firstLayerWeight + learning_rate * firtLayerGradient
  }
  #-------------------------------------------------------------#
  # Printing the final training loss
  print(loss)
  
}

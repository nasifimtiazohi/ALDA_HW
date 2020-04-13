########
# HW4 
# Instructor: Dr. Thomas Price
# Specify your team details here
# 
#
#
#
#########
library(dplyr)
library(ggplot2)

alda_calculate_sse <- function(data_df, cluster_assignments){
  # Calculate overall SSE
  # This code has already been given to you
  # Input:
    # data_df: data frame that has been given to you by the TA (x,y attributes)
    # cluster_assignments: a vector of cluster assignments (e.g. 1, 2), each corresponding to a row in the dataframe,
    #                      which have been generated using any of the clustering algorithms
  # Output:
    # A single value of type double, which is the total SSE of the clusters, using Euclidean distance.
    # To calculate the SSE, first calculate the centroid for the cluster, then calculate to total Euclidean
    # distance (error) from each point to that centroid.

  all_data <- data.frame(data_df, cluster_assignments)
  all_data <- all_data %>%
    group_by(cluster_assignments) %>%
    summarize(within_sse=sum((x - mean(x))^2 + (y - mean(y))^2)) %>%
    summarize(total_sse=sum(within_sse))
  return(all_data$total_sse)
}



alda_kmeans_elbow_plot <- function(data_df, k_values){
  # ~ 8-10 lines
  # Input:
    # data_df: Original data frame supplied to you by the TA
    # k_values: A vector of values of k for k means clustering
  
  # General Information:
    # Run k means for all the values specified in k_values, generate elbow plot
    # Use alda_cluster with kmeans as your clustering type
    # (you can see an example this function call in hw4_checker.R for k = 2, now repeat it for all k_values)
  
  # Output:
    # Nothing, simply generate a plot and save it to disk as "GroupNumber_elbow.png"
  mat<-matrix(,0,2)
 for(k in k_values){
   kmeans_result <- alda_cluster(data_df, k, "kmeans")
   #print(kmeans_result)
   kmeans_sse <- alda_calculate_sse(data_df, kmeans_result)
   mat <- rbind(mat,list(k,kmeans_sse))
 }
  df<- as.data.frame(mat)
  png('14_elbow.png')
  plot(df, type="b", xlab="Number of clusters", ylab = "Sum of Squared Error")
  dev.off()
}


alda_cluster <- function(data_df, n_clusters, clustering_type){
  cluster_center <- matrix(c(1,1,-1,-1), ncol=2, byrow=TRUE) # We provide this as the initial points for kmeans.
  # Perform specified clustering
  
  # Inputs:
  # data_df: The dataset provided to you, 2-dimensional (x1,x2)
  # n_clusters: number of clusters to be created, in the case of kmeans it is the starting centers for the clusters.
  # clustering_type: can be one of "kmeans" or "single-link" or "complete-link"
  
  # Outputs:
  # Cluster assignments for the specified method (vector, with length = nrow(data_df) and values ranging from 1 to n_clusters)
  print(n_clusters)
  if(clustering_type == "kmeans"){
    # ~ 1-2 lines
    # allowed packages for kmeans: R-base, stats, dplyr
    # set the max number of iterations to 100, number of random restarts = 1 (let's not break the TA's computer! )
    # choose "Lloyd" as the algorithm 
    
    result <- kmeans(data_df, n_clusters,100,1,"Lloyd")
    return(result$cluster)
    
    
  }else if(clustering_type == "single-link"){
    # ~ 3-5 lines
    # Allowed packages for single-link: R-base, stats, dplyr
    # Use euclidean distance for distance calculation (Hint: Look at dist method from stats package)
    # Note 1: Can you use the data_df directly for hclust, or do you need to compute something first?
            # What does 'd' mean in hclust? 
    # Note 2: Does hclust return the clusters assignments directly, or does it return a dendrogram? 
            # Hint 2: Look up the stats package for a method to cut the tree at n_clusters
            # Visualize the dendrogram - paste this dendrogram in your PDF 
    
    d <- dist(data_df, "euclidean")
    singleLinkTree <- hclust(d, method="single")
    #plot(singleLinkTree)
    
    result <- cutree(singleLinkTree,n_clusters)
    plot(singleLinkTree)
    return(result)
    
    
    
  }else{ #complete link clustering is default
    # ~ 3-5 lines
    # Allowed packages for single-link: R-base, stats, dplyr
    # Use euclidean distance for distance calculation (Hint: Look at dist method from stats package)
    # Note 1: Can you use the data_df directly for hclust, or do you need to compute something first?
    # What does 'd' mean in hclust? 
    # Note 2: Does hclust return the clusters assignments directly, or does it return a dendrogram? 
    # Hint 2: Look up the stats package for a method to cut the dendrogram at n_clusters
    # Visualize the dendrogram - paste this dendrogram in your PDF 
    
    d <- dist(data_df, method="euclidean")
    completeLinkTree <- hclust(d, method = "complete")
    plot(completeLinkTree)
    
    result <- cutree(completeLinkTree, n_clusters)
    return(result)
  }
}



alda_svm <- function(x_train, x_test, y_train, kernel_name){
  # Perform classification using support vector machines (linear/radial/sigmoid)
  
  # Inputs:
  # x_train: training data frame(4 variables, x1-x4)
  # x_test: test data frame(4 variables, x1-x4)
  # y_train: dependent variable, training data (factor)
  # kernel_name: specifies type of SVM kernel, string variable, can be of type 'linear', 'radial' or 'sigmoid' or 'polynomial'
  
  # General information
  # Both training data and test data have already been scaled - so you don't need to scale it once again.
  
  # Kernel specific information: using 10-fold cross-validation, perform hyperparameter tuning for each kernel as shown below:
  # Linear: 
  # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10)
  # radial: 
  # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
  # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
  # polynomial:
  # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
  # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
  # 'degree' parameter: for the following values: c(1,2,3)
  # sigmoid:
  # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
  # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
  
  # Output:
  # A list with two elements, first element = model generated, second element = predictions on test data (factor) 
  
  # Word of caution:
  # Make sure that you pick the best parameters after tuning
  
  # allowed packages: R-base, e1071
  
  
  if(kernel_name == "radial"){
    # ~1-2 lines 
    svm_tune <- tune(svm, x_train, y_train, kernel="radial",ranges = list(gamma=c(0.01, 0.1, 1, 10), cost = c(0.01, 0.1, 1, 10)))
    best_cost <- svm_tune$best.parameters$cost
    best_gamma <- svm_tune$best.parameters$gamma
    svm_after_tune <- svm(x_train, y_train, kernel="radial", cost=best_cost,gamma = best_gamma)
    
    prediction <- predict(svm_after_tune, x_test)
    return(list(svm_after_tune,as.factor(prediction)))
  }else if(kernel_name == 'polynomial'){
    #~1-2 lines
    
    svm_tune <- tune(svm, x_train, y_train, kernel="polynomial",ranges = list(gamma=c(0.05, 0.5, 1, 2), cost = c(0.01, 0.1, 1, 10), degree=c(1,2,3)))
    best_cost <- svm_tune$best.parameters$cost
    best_gamma <- svm_tune$best.parameters$gamma
    best_degree <-svm_tune$best.parameters$degree
    svm_after_tune <- svm(x_train, y_train, kernel="polynomial", cost=best_cost,gamma = best_gamma, degree = best_degree)
    
    prediction <- predict(svm_after_tune, x_test)
    return(list(svm_after_tune,as.factor(prediction)))

  }else if(kernel_name == 'sigmoid'){
    #~1-2 lines
    svm_tune <- tune(svm, x_train, y_train, kernel="sigmoid",ranges = list(gamma=c(0.05, 0.5, 1, 2), cost = c(0.01, 0.1, 1, 10)))
    best_cost <- svm_tune$best.parameters$cost
    best_gamma <- svm_tune$best.parameters$gamma
    svm_after_tune <- svm(x_train, y_train, kernel="sigmoid", cost=best_cost,gamma = best_gamma)
    
    prediction <- predict(svm_after_tune, x_test)
    return(list(svm_after_tune,as.factor(prediction)))
    
  }else{ # default linear kernel
    #~1-2 lines
    svm_tune <- tune(svm, x_train, y_train, kernel="linear",ranges = list(cost = c(0.01, 0.1, 1, 10)))
    best_cost <- svm_tune$best.parameters$cost
    svm_after_tune <- svm(x_train, y_train, kernel="linear", cost=best_cost)
    
    prediction <- predict(svm_after_tune, x_test)
    return(list(svm_after_tune,as.factor(prediction)))

  }
  
}

classification_compare_accuracy <- function(y_test, linear_kernel_prediction, radial_kernel_prediction, 
                                            polynomial_kernel_prediction, sigmoid_kernel_prediction){
  # ~ 6-10 lines of code
  # Calculate the accuracy for each of the classification methods: 
  # 'svm-linear': linear kernel SVM
  # 'svm-radial': radial kernel SVM
  # 'svm-poly': polynomial kernel SVM
  # 'svm-sigmoid': sigmoid kernel SVM 
  # Return the best method and its accuracy (i.e., method with highest accuracy)
  
  # Inputs:
  # y_test: ground truth dependent variable from test data (factor)
  # linear_kernel_prediction: predictions from linear kernel SVM (factor)
  # radial_kernel_prediction: predictions from radial kernel SVM (factor)
  # polynomial_kernel_prediction: predictions from polynomial kernel SVM (factor)
  # sigmoid_kernel_prediction: predictions from sigmoid kernel SVM (factor)
  
  # Returns:
  # list of three values:
  # First value, of type string, with the name of the best method, sould be:
  # 'svm-linear' if linear_kernel_prediction is best
  # 'svm-radial' if radial_kernel_prediction is best
  # 'svm-poly' if polynomial_kernel_prediction is best
  # 'svm-sigmoid' if sigmoid_kernel_prediction is best
  # Second value, of type double, with the corresponding overall accuracy of the best method (on a scale of 100, do not round off)
  # third value, a vector with the overall accuracies of all methods in this order: c(linear-svm's accuracy, radial-svm's accuracy, poly-svm's accuracy, sigmoid-svm's accuracy)
  # Allowed packages: R-base
  # Note that I asked you to implement accuracy calculation - do not use a library for this
  
  df_linear <- table(y_test, linear_kernel_prediction)
  linear_acc<- (df_linear[1,1]+df_linear[2,2])/(sum(df_linear))
  #print(linear_acc)
  
  
  df_radial <- table(y_test, radial_kernel_prediction)
  radial_acc<- (df_radial[1,1]+df_radial[2,2])/(sum(df_radial))
  #print(radial_acc)
  
  df_sigmoid <- table(y_test, sigmoid_kernel_prediction)
  sigmoid_acc<- (df_sigmoid[1,1]+df_sigmoid[2,2])/(sum(df_sigmoid))
  #print(sigmoid_acc)
  
  df_poly <- table(y_test, polynomial_kernel_prediction)
  poly_acc<- (df_poly[1,1]+df_poly[2,2])/(sum(df_poly))
  #print(poly_acc)
  accuracies <- c(linear_acc, radial_acc, poly_acc, sigmoid_acc)*100
  #print(accuracies)
  
  best_svm_kernel= switch(which.max(accuracies),"svm-linear","svm-radial","svm-poly","svm-sigmoid")
  return(list(best_svm_kernel, max(accuracies),accuracies))
}

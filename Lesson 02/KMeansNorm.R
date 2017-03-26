# KMeansNorm.R

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  #browser()
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
    # 3(a) Get mean and standard deviation of point dimensions. Use the mean and sd functions
    obs_mean_1 <- mean(observations[,1]) # Mean
    obs_sd_1 <- sd(observations[,1]) #Standard Deviation
    # normalize 1st dimension of observations
    # 3(b) Z-Normalize points and centroid guesses based on distribution of points
    obs_z_norm_1 <- (observations[,1] - obs_mean_1)/obs_sd_1 #calculating zscore
    # normalize 1st dimension of clusterCenters
    #clust_cent_mean_1 <- mean(clusterCenters[,1])
    #clust_cent_sd_1 <- sd(clusterCenters[,1])
    clust_cent_z_norm_1 <- (clusterCenters[,1] - obs_mean_1)/obs_sd_1 #calculating zscore
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations  
    # 3(a) Get mean and standard deviation of point dimensions. Use the mean and sd functions
    obs_mean_2 <- mean(observations[,2])
    obs_sd_2 <- sd(observations[,2])
    # normalize 2nd dimension of observations
    # 3(b) Z-Normalize points and centroid guesses based on distribution of points
    obs_z_norm_2 <- (observations[,2] - obs_mean_2)/obs_sd_2 #calculating zscore
    # normalize 2nd dimension of clusterCenters
    #clust_cent_mean_2 <- mean(clusterCenters[,2])
    #clust_cent_sd_2 <- sd(clusterCenters[,2])
    clust_cent_z_norm_2 <- (clusterCenters[,2] - obs_mean_2)/obs_sd_2 #calculating zscore
  }
  # This clustering is not using a normalised input.
  if (!normD1 & !normD2) # Test 1
  {
    clusterCenters <- KMeans(observations, clusterCenters)
    return(clusterCenters)
  }
  # 3(c)Let the KMeans function in Kmeans.R determine the centroids in normalized space
  if (normD1 & normD2) # Test # 4
  {  
    clust_cent_norm <- as.matrix(data.frame(clust_cent_z_norm_1, clust_cent_z_norm_2))
    obs_z_norm <- as.matrix(data.frame(obs_z_norm_1, obs_z_norm_2))
    # This clustering is using normalised input.
    clusterCenters_norm <- KMeans(obs_z_norm, clust_cent_norm)
    return(clusterCenters_norm)
    #3(d)De-normalize the centroids obtained from step 3(c)
    #clusterCenters_denorm_1 <- clusterCenters_norm[,1]*obs_sd_1 + obs_mean_1
    #clusterCenters_denorm_2 <- clusterCenters_norm[,1]*obs_sd_2 + obs_mean_2
    #clusterCenters_denorm <- data.frame(clusterCenters_denorm_1, clusterCenters_denorm_2)
    #names(clusterCenters_denorm) <- c('V1', 'V2')
    #clusterCenters_denorm_mat <- as.matrix(clusterCenters_denorm)# Convert data fram to a matrix so it can be processed by kmeans function.  
    #return(clusterCenters_denorm_mat)
    # Checking calulation at matrix level
    #obs_mean_matrix <- mean(observations) # Mean
    #obs_sd_matrix <- sd(observations) #Standard Deviation
    #clusterCenters_denorm_matrix <- clusterCenters_norm*obs_sd_matrix + obs_mean_matrix
    #return(clusterCenters_denorm_matrix)
    #clusterCenters_denorm_1 <- clusterCenters_norm[,1]*obs_sd_1 + obs_mean_1
    #clusterCenters_denorm_2 <- clusterCenters_norm[,1]*obs_sd_2 + obs_mean_2
    #clusterCenters_denorm <- data.frame(clusterCenters_denorm_1, clusterCenters_denorm_2)
    #names(clusterCenters_denorm) <- c('V1', 'V2')
    #clusterCenters_denorm_mat <- as.matrix(clusterCenters_denorm)# Convert data fram to a matrix so it can be processed by kmeans function.  
    #return(clusterCenters_denorm_mat)
  }
  # Test # 2
  if (normD1 & !normD2) 
  {  
    clust_cent_norm1_notnorm2 <- as.matrix(data.frame(clust_cent_z_norm_1, clusterCenters[,2]))
    obs_z_norm1_notnorm2 <- as.matrix(data.frame(obs_z_norm_1, observations[,2]))
    clusterCenters_norm1notnorm2 <- KMeans(obs_z_norm1_notnorm2, clust_cent_norm1_notnorm2)
    return(clusterCenters_norm1notnorm2)

  }
  #Test # 3
  if (!normD1 & normD2)
  {  
    clust_cent_notnorm1_norm2 <- as.matrix(data.frame(clusterCenters[,1],clust_cent_z_norm_2))
    obs_z_notnorm1_norm2 <- as.matrix(data.frame(observations[,1],obs_z_norm_2))
    clusterCenters_notnorm1_norm2 <- KMeans(obs_z_notnorm1_norm2, clust_cent_notnorm1_norm2)
    return(clusterCenters_notnorm1_norm2)
    
  }
  
}


# 4(a) What is the single most obvious difference between the distributions of the first and second dimensions?
# Answer : 2nd dimension has a much wider range as compared to the 1st dimension

# 4(b) Does clustering in Test 1 occur along one or two dimensions? Which dimensions? Why?
# Answer : Yes, clustering requires both dimensions, in this test both the inputs are 
# not normalized as the function is using the unprocessed input supplied from KMeansNormTest.R.

# 4(c) Does clustering in Test 2 occur along one or two dimensions? Which dimensions? Why?
# Answer : No, clustering requires both dimensions, in this test normalised input from second 
# dimension is not available as it has not been processed due to this input argument normD2=F

# 4(d) Does clustering in Test 3 occur along one or two dimensions? Which dimensions? Why?
# Answer : No, clustering requires both dimensions, in this test normalised input from first 
# dimension is not available as it has not been processed due to this input argument normD1=F

# 4(e) Does clustering in Test 4 occur along one or two dimensions? Which dimensions? Why?
# Answer : Yes, in this case normalised input from both the dimensions are available.

# 5 Why is normalization important in K-means clustering?
# Answer : Normalization is important to put data on equal terms.

# 6 How do you encode categorical data in a K-means clustering?
# Answer : Categorical data can be binarized.

# 7 Why is clustering un-supervised learning as opposed to supervised learning?
# Answer : K-means is unsupervised because we do not tell the algorithm what outcome was observed or what outcome is 
# desired.

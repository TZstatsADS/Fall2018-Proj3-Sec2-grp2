#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng/Siyu Zhu
### Project 3

## Test code ##########
LR_dir <- "/Users/siyuzhu/Documents/Github/ADS/Fall2018-Proj3-Sec2--sec2proj3_grp2/data/train_set/LR/"
HR_dir <- "/Users/siyuzhu/Documents/Github/ADS/Fall2018-Proj3-Sec2--sec2proj3_grp2/data/train_set/HR/"
n_points=1000

# function to locate the samples' coordinates in the imgLR matrix 
locate <- function(n, ncol) {
  row <- floor(n/ncol) + 1
  col <- n - ncol * (row - 1)
  # corner case
  if(col == 0) {
    row = row - 1
    col = ncol
  }
  return(c(row, col))
}

# function to find the value of an element in matrix given its location
# find <- function(loc, matrix) {
#   return(matrix[loc[1], loc[2]])
# }
# 
# find2 <- function(loc, matrix, ch) {
#   return(matrix[loc[1], loc[2], ch])
# }
#######################

feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir)) # 1500
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    LR_nrow <- nrow(imgLR)
    LR_ncol <- ncol(imgLR)
    HR_nrow <- 2 * LR_nrow
    HR_ncol <- 2 * LR_ncol
    
    ### step 3. For three channels
    for (k in c(1:3)) {
      
      ### step 1. sample n_points frolm imgLR
      sampled_points <- sample(LR_nrow * LR_ncol, n_points)
      sampled_locations <- t(sapply(sampled_points, locate, LR_ncol)) # locations in the imgLR matrix
      new_locations <- sampled_locations + 1 # locations in the supp matrix
      HR_locations <- 2 * sampled_locations - 1  # corresponding locations in the imgHR matrix
      
      # supplementary image matrix
      supp_imgLR <- cbind(imgLR[, 1, k], imgLR[, , k], imgLR[, LR_ncol, k])
      supp_imgLR <- rbind(cbind(imgLR[1, 1, k], imgLR[1, , k],imgLR[1, LR_ncol, k]), 
                          supp_imgLR, cbind(imgLR[LR_nrow, 1, k], imgLR[LR_nrow, , k],imgLR[LR_nrow, LR_ncol, k]))
      supp_imgHR <- imgHR[, , k]
      ### step 2. fill the featM and the labM
      ## for featM
      # j = 1
      loc1 <- cbind(new_locations[, 1] - 1, new_locations[, 2] - 1)
      featMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  1, k] <- diag(supp_imgLR[loc1[, 1], loc1[,2]])
      loc2 <- cbind(new_locations[, 1] - 1, new_locations[, 2])
      featMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  2, k] <- diag(supp_imgLR[loc2[, 1], loc2[,2]])
      # j = 3
      loc3 <- cbind(new_locations[, 1] - 1, new_locations[, 2] + 1)
      featMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  3, k] <- diag(supp_imgLR[loc3[, 1], loc3[,2]])
      # j = 4
      loc4 <- cbind(new_locations[, 1], new_locations[, 2] - 1)
      featMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  4, k] <- diag(supp_imgLR[loc4[, 1], loc4[,2]])
      # j = 5
      loc5 <- cbind(new_locations[, 1], new_locations[, 2] + 1)
      featMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  5, k] <- diag(supp_imgLR[loc5[, 1], loc5[,2]])
      # j = 6
      loc6 <- cbind(new_locations[, 1] + 1, new_locations[, 2] - 1)
      featMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  6, k] <- diag(supp_imgLR[loc6[, 1], loc6[,2]])
      # j = 7
      loc7 <- cbind(new_locations[, 1] + 1, new_locations[, 2])
      featMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  7, k] <- diag(supp_imgLR[loc7[, 1], loc7[,2]])
      # j = 8
      loc8 <- cbind(new_locations[, 1] + 1, new_locations[, 2] + 1)
      featMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  8, k] <- diag(supp_imgLR[loc8[, 1], loc8[,2]])
      
      ## for labM
      
      # j = 1
      location1 <- HR_locations
      labMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  1, k] <- diag(supp_imgHR[location1[, 1], location1[,2]])
      # j = 2
      location2 <- cbind(HR_locations[, 1], HR_locations[, 2] + 1)
      labMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  2, k] <- diag(supp_imgHR[location2[, 1], location2[,2]])
      # j = 3
      location3 <- cbind(HR_locations[, 1] + 1, HR_locations[, 2])
      labMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  3, k] <- diag(supp_imgHR[location3[, 1], location3[,2]])
      # j = 4
      location4 <- cbind(HR_locations[, 1] + 1, HR_locations[, 2] + 1)
      labMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  4, k] <- diag(supp_imgHR[location4[, 1], location4[,2]])
      #########
      print(paste("k = ", k, sep = ""))
    }
    print(paste("i = ", i, sep = ""))
  }
  return(list(feature = featMat, label = labMat))
}







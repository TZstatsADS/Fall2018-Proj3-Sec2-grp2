#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng/Siyu Zhu
### Project 3

## Test code ##########
LR_dir <- "/Users/siyuzhu/Documents/Github/ADS/Fall2018-Proj3-Sec2--sec2proj3_grp2/data/train_set/LR/"
HR_dir <- "/Users/siyuzhu/Documents/Github/ADS/Fall2018-Proj3-Sec2--sec2proj3_grp2/data/train_set/HR/"
n_points=1000

# function to locate the samples' corrdinates in the imgLR matrix 
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

#function to fill featMat
distribute1 <- function(vec, img = supp_imgLR){
  row_index <- vec[1]
  col_index <- vec[2]
  cent_value <- img[row_index, col_index]
  return(c(img[row_index - 1, col_index - 1] - cent_value,
           img[row_index - 1, col_index] - cent_value,
           img[row_index - 1, col_index + 1] - cent_value,
           img[row_index, col_index - 1] - cent_value,
           img[row_index, col_index + 1] - cent_value,
           img[row_index + 1, col_index - 1] - cent_value,
           img[row_index + 1, col_index] - cent_value,
           img[row_index + 1, col_index + 1] - cent_value))
}

#function to fill labMat
distribute2 <- function(vec, img = imgHR, img2 = supp_imgLR, ch = k){
  Hrow_index <- vec[1]
  Hcol_index <- vec[2]
  row2 <- (Hrow_index + 1) / 2 + 1
  col2 <- (Hcol_index + 1) / 2 + 1
  cent_value <- img2[row2, col2]
  return(c(img[Hrow_index, Hcol_index, ch] - cent_value,
           img[Hrow_index, Hcol_index + 1, ch] - cent_value,
           img[Hrow_index + 1, Hcol_index, ch] - cent_value,
           img[Hrow_index + 1, Hcol_index + 1, ch] - cent_value))
}

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
  for(i in c(1:1)){
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
      HR_locations <- 2 * sampled_locations - 1  # corresponding locations in the imgHR matrix
      new_locations <- sampled_locations + 1 # locations in the supp matrix
      
      # supplementary image matrix
      supp_imgLR <- cbind(imgLR[, 1, k], imgLR[, , k], imgLR[, LR_ncol, k])
      supp_imgLR <- rbind(cbind(imgLR[1, 1, k], imgLR[1, , k],imgLR[1, LR_ncol, k]), 
                          supp_imgLR, cbind(imgLR[LR_nrow, 1, k], imgLR[LR_nrow, , k],imgLR[LR_nrow, LR_ncol, k]))
       
      ### step 2. fill the featM and the labM
      featMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  , k] <- t(apply(new_locations, 1, distribute1, img = supp_imgLR))
      labMat[((i - 1) * n_points + 1) : ((i - 1) * n_points + n_points),  , k] <- t(apply(HR_locations, 1, distribute2, img = imgHR, img2 = supp_imgLR, ch = k))
      #########
      print(paste("k = ", k, sep = ""))
     }
  print(paste("i = ", i, sep = ""))
  }
  return(list(feature = featMat, label = labMat))
}




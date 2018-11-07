#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

# Default feature
feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR)
    imgHR = as.array(imgHR)
    ### step 1. sample n_points from imgLR
    dimLR=dim(imgLR)
    select=sample(dimLR[1]*dimLR[2],n_points)
    select_row=(select-1)%%dimLR[1]+1
    select_col=(select-1)%/%dimLR[1]+1
    #summary(selectLR)
    ### step 2. for each sampled point in imgLR,
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    for(j in 1:3){
      pad=cbind(0,imgLR[,,j],0)
      pad=rbind(0,pad,0)
      center=pad[cbind(select_row+1,select_col+1)]
      featMat[(i-1)*n_points+1:n_points,1,j]=pad[cbind(select_row,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,2,j]=pad[cbind(select_row,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,3,j]=pad[cbind(select_row,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,4,j]=pad[cbind(select_row+1,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,5,j]=pad[cbind(select_row+2,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,6,j]=pad[cbind(select_row+2,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,7,j]=pad[cbind(select_row+2,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,8,j]=pad[cbind(select_row+1,select_col)]-center
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      channelHR=imgHR[,,j]
      
      labMat[(i-1)*n_points+1:n_points,1,j]=channelHR[cbind(select_row*2-1,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,2,j]=channelHR[cbind(select_row*2-1,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,3,j]=channelHR[cbind(select_row*2,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,4,j]=channelHR[cbind(select_row*2,select_col*2-1)]-center
      ### step 3. repeat above for three channels
    }
  }
  return(list(feature = featMat, label = labMat))
}

# Hog Feature
feature_hog <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  library("OpenImageR")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- matrix(NA, n_files, 54) 
  labMat <- matrix(NA, n_files, 54) 
  
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    featMat[i,] <- HOG(imgLR)
    labMat[i,] <-HOG(imgHR)
  }
  return(list(feature = featMat, label = labMat))
}


# RGB feature
feature_rgb <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ### load libraries
  library("EBImage")
  library(grDevices)
  ### Define the number of R, G and B
  n_files <- length(list.files(LR_dir))
  nR <- 10
  nG <- 14
  nB <- 14 
  rBin <- seq(0, 1, length.out=nR)
  gBin <- seq(0, 1, length.out=nG)
  bBin <- seq(0, 1, length.out=nB)
  imgLR_mat = array()
  imgHR_mat = array()
  imgLR_freq_rgb = array()
  imgHR_freq_rgb = array()
  featMat = matrix(nrow=n_files, ncol=nR*nG*nB)
  labMat = matrix(nrow=n_files, ncol=nR*nG*nB)
  
  
  ### extract RGB features
  for (i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR_mat <- imageData(imgLR)
    imgHR_mat <- imageData(imgHR)
    imgLR_as_rgb <-array(c(imgLR_mat,imgLR_mat,imgLR_mat),dim = c(nrow(imgLR_mat),ncol(imgLR_mat),3))
    imgHR_as_rgb <-array(c(imgHR_mat,imgHR_mat,imgHR_mat),dim = c(nrow(imgHR_mat),ncol(imgHR_mat),3))
    imgLR_freq_rgb <- as.data.frame(table(factor(findInterval(imgLR_as_rgb[,,1], rBin), levels=1:nR), 
                                          factor(findInterval(imgLR_as_rgb[,,2], gBin), levels=1:nG),
                                          factor(findInterval(imgLR_as_rgb[,,3], bBin), levels=1:nB)))
    imgHR_freq_rgb <- as.data.frame(table(factor(findInterval(imgHR_as_rgb[,,1], rBin), levels=1:nR), 
                                          factor(findInterval(imgHR_as_rgb[,,2], gBin), levels=1:nG),
                                          factor(findInterval(imgHR_as_rgb[,,3], bBin), levels=1:nB)))
    
    featMat[i,] <- as.numeric(imgLR_freq_rgb$Freq)/(ncol(imgLR_mat)*nrow(imgLR_mat)) # normalization
    labMat[i,] <- as.numeric(imgHR_freq_rgb$Freq)/(ncol(imgHR_mat)*nrow(imgHR_mat)) # normalization
  }
  return(list(feature = featMat, label = labMat))
}

# Canny feature
feature_canny <- function(LR_dir, HR_dir, n_points=1000){
  library("EBImage")
  library("imager")
  n_files <- length(list.files(LR_dir))
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  for (i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- as.array(imgHR)
    
    imgLR <- imgLR %>% as.cimg %>% cannyEdges %>% as.cimg
    dimLR <- dim(imgLR)
    
    
    s1 <- sample(which(imgLR[,,1]==1),n_points*0.6,replace=T)
    s2 <- sample(which(imgLR[,,1]==0),n_points*0.4,replace=T)
    select <- c(s1,s2)
    
    select_row=(select-1)%%dimLR[1]+1
    select_col=(select-1)%/%dimLR[1]+1
    
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR)
    
    for(j in 1:3){
      pad=cbind(0,imgLR[,,j],0)
      pad=rbind(0,pad,0)
      center=pad[cbind(select_row+1,select_col+1)]
      featMat[(i-1)*n_points+1:n_points,1,j]=pad[cbind(select_row,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,2,j]=pad[cbind(select_row,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,3,j]=pad[cbind(select_row,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,4,j]=pad[cbind(select_row+1,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,5,j]=pad[cbind(select_row+2,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,6,j]=pad[cbind(select_row+2,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,7,j]=pad[cbind(select_row+2,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,8,j]=pad[cbind(select_row+1,select_col)]-center
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      channelHR=imgHR[,,j]
      
      labMat[(i-1)*n_points+1:n_points,1,j]=channelHR[cbind(select_row*2-1,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,2,j]=channelHR[cbind(select_row*2-1,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,3,j]=channelHR[cbind(select_row*2,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,4,j]=channelHR[cbind(select_row*2,select_col*2-1)]-center
      ### step 3. repeat above for three channels
    }
  }
  return(list(feature = featMat, label = labMat))
  
}




# Outsider feature
feature_diagonal <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 4, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR)
    imgHR = as.array(imgHR)
    ### step 1. sample n_points from imgLR
    dimLR=dim(imgLR)
    select=sample(dimLR[1]*dimLR[2],n_points)
    select_row=(select-1)%%dimLR[1]+1
    select_col=(select-1)%/%dimLR[1]+1
    #summary(selectLR)
    ### step 2. for each sampled point in imgLR,
    ### step 2.1. save (the neighbor 24 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    for(j in 1:3){
      pad=cbind(0,imgLR[,,j],0)
      pad=rbind(0,pad,0)
      center=pad[cbind(select_row+1,select_col+1)]
      
      
      featMat[(i-1)*n_points+1:n_points,1,j]=pad[cbind(select_row,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,2,j]=pad[cbind(select_row,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,3,j]=pad[cbind(select_row+2,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,4,j]=pad[cbind(select_row+2,select_col+2)]-center
      
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      channelHR=imgHR[,,j]
      
      labMat[(i-1)*n_points+1:n_points,1,j]=channelHR[cbind(select_row*2-1,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,2,j]=channelHR[cbind(select_row*2-1,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,3,j]=channelHR[cbind(select_row*2,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,4,j]=channelHR[cbind(select_row*2,select_col*2-1)]-center
      ### step 3. repeat above for three channels
    }
  }
  return(list(feature = featMat, label = labMat))
}


# 24 pixels feature
feature_diagonal <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 4, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR)
    imgHR = as.array(imgHR)
    ### step 1. sample n_points from imgLR
    dimLR=dim(imgLR)
    select=sample(dimLR[1]*dimLR[2],n_points)
    select_row=(select-1)%%dimLR[1]+1
    select_col=(select-1)%/%dimLR[1]+1
    #summary(selectLR)
    ### step 2. for each sampled point in imgLR,
    ### step 2.1. save (the neighbor 24 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    for(j in 1:3){
      pad=cbind(0,0,imgLR[,,j],0,0)
      pad=rbind(0,0,pad,0,0)
      center=pad[cbind(select_row+2,select_col+2)]
      
      
      featMat[(i-1)*n_points+1:n_points,1,j]=pad[cbind(select_row,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,2,j]=pad[cbind(select_row,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,3,j]=pad[cbind(select_row,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,4,j]=pad[cbind(select_row,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,5,j]=pad[cbind(select_row,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,6,j]=pad[cbind(select_row+1,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,7,j]=pad[cbind(select_row+2,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,8,j]=pad[cbind(select_row+3,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,9,j]=pad[cbind(select_row+4,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,10,j]=pad[cbind(select_row+4,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,11,j]=pad[cbind(select_row+4,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,12,j]=pad[cbind(select_row+4,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,13,j]=pad[cbind(select_row+4,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,14,j]=pad[cbind(select_row+3,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,15,j]=pad[cbind(select_row+2,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,16,j]=pad[cbind(select_row+1,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,17,j]=pad[cbind(select_row+1,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,18,j]=pad[cbind(select_row+1,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,19,j]=pad[cbind(select_row+1,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,20,j]=pad[cbind(select_row+2,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,21,j]=pad[cbind(select_row+3,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,22,j]=pad[cbind(select_row+3,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,23,j]=pad[cbind(select_row+3,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,24,j]=pad[cbind(select_row+2,select_col+1)]-center
      
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      channelHR=imgHR[,,j]
      
      labMat[(i-1)*n_points+1:n_points,1,j]=channelHR[cbind(select_row*2-1,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,2,j]=channelHR[cbind(select_row*2-1,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,3,j]=channelHR[cbind(select_row*2,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,4,j]=channelHR[cbind(select_row*2,select_col*2-1)]-center
      ### step 3. repeat above for three channels
    }
  }
  return(list(feature = featMat, label = labMat))
}


#outsider feature
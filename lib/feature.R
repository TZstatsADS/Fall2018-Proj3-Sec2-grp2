#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

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
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"),type="jpeg")
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"),type="jpeg")
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


############# HoG ##############
feature_hog<-function(LR_dir, HR_dir, export=TRUE){
  
  ### Construct process features for training/testing images
  ### HOG: calculate the Histogram of Oriented Gradient for an image
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  
  ### load libraries
  library("EBImage")
  library("OpenImageR")
  
  dir_names_L <- list.files(LR_dir)
  n_files_L <- length(dir_names_L)
  
  ### calculate HOG of images
  dat_L <- matrix(NA, n_files_L, 54) 
  for(i in 1:n_files_L){
    img_L <- readImage(paste0(LR_dir,  dir_names_L[i]))
    dat_L[i,] <- HOG(img_L)
  }
  
  dir_names_H <- list.files(HR_dir)
  n_files_H <- length(dir_names_H)
  
  ### calculate HOG of images
  dat_H <- matrix(NA, n_files_H, 54) 
  for(i in 1:n_files_H){
    img_H <- readImage(paste0(HR_dir,  dir_names_H[i]))
    dat_H[i,] <- HOG(img_H)
  }
  
  
  ### output constructed features
  #if(export){
   # save(list(feature_L = dat_L, feature_H = dat_H), file=paste0("../output/HOG.RData"))
  # }
  return(list(feature_L = dat_L, feature_H = dat_H))
}




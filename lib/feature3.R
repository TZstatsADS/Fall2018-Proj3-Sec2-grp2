#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3
# LR_dir <- "/Users/siyuzhu/Documents/Github/ADS/Fall2018-Proj3-Sec2--sec2proj3_grp2/data/train_set/LR/"
# HR_dir <- "/Users/siyuzhu/Documents/Github/ADS/Fall2018-Proj3-Sec2--sec2proj3_grp2/data/train_set/HR/"
# n_points=1000

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
    #locate
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



############# RGB ##############
feature_rgb <- function(img_dir, export=T){
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ### load libraries
  library("EBImage")
  library(grDevices)
  ### Define the b=number of R, G and B
  nR <- 10
  nG <- 14
  nB <- 14 
  rBin <- seq(0, 1, length.out=nR)
  gBin <- seq(0, 1, length.out=nG)
  bBin <- seq(0, 1, length.out=nB)
  mat=array()
  freq_rgb=array()
  img_names = list.files(img_dir)
  img_n = length(img_names)
  rgb_feature=matrix(nrow=img_n, ncol=nR*nG*nB)
  
  n_files <- length(list.files(img_dir))
  
  ### extract RGB features
  for (i in 1:img_n){
    mat <- imageData(readImage(paste0(img_dir, img_names[i])))
    mat_as_rgb <-array(c(mat,mat,mat),dim = c(nrow(mat),ncol(mat),3))
    freq_rgb <- as.data.frame(table(factor(findInterval(mat_as_rgb[,,1], rBin), levels=1:nR), 
                                    factor(findInterval(mat_as_rgb[,,2], gBin), levels=1:nG),
                                    factor(findInterval(mat_as_rgb[,,3], bBin), levels=1:nB)))
    rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
    
    mat_rgb <-mat_as_rgb
    dim(mat_rgb) <- c(nrow(mat_as_rgb)*ncol(mat_as_rgb), 3)
  }
  
  ### output RGB features
  if(export){
    save(rgb_feature, file = "../output/rgb_feature_new3.RData")
  }
  return(data.frame(rgb_feature))
}





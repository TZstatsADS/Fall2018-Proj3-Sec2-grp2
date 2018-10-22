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
  for (k in 1:3){
    
    for(i in 1:n_files){
      imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
      imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
      ### step 1. sample n_points from imgLR
      
      points_postion <- order(sample(imgLR,n_points))  
      
      ### step 2. for each sampled point in imgLR,
      for (j in 1:length(points_postion)){
        cent_pix <- imgLR[points_postion[j]]
        leftside_index <- seq(251,250*248+1,250)
        rightside_index <-seq(500,250*249,250)
        # From 2 to 249 ignore the upper three neigh to zero
        if (points_postion[j]>1 && points_postion[j]<250){
          neigh_pix <- c(0,
                         0,
                         0,
                         imgLR[points_postion[j]-1]-cent_pix,
                         imgLR[points_postion[j]+1]-cent_pix,
                         imgLR[points_postion[j]+249]-cent_pix,
                         imgLR[points_postion[j]+250]-cent_pix,
                         imgLR[points_postion[j]+251]-cent_pix)
        }else if (points_postion[j]>62251 && points_postion[j]<250*250){
          neigh_pix <- c(imgLR[points_postion[j]-251]-cent_pix,
                         imgLR[points_postion[j]-250]-cent_pix,
                         imgLR[points_postion[j]-249]-cent_pix,
                         imgLR[points_postion[j]-1]-cent_pix,
                         imgLR[points_postion[j]+1]-cent_pix,
                         0,
                         0,
                         0)
          #Ignore the lower three neigh to zero
        }else if(points_postion[j] %in% leftside_index){
          neigh_pix <- c(0,
                         imgLR[points_postion[j]-250]-cent_pix,
                         imgLR[points_postion[j]-249]-cent_pix,
                         0,
                         imgLR[points_postion[j]+1]-cent_pix,
                         0,
                         imgLR[points_postion[j]+250]-cent_pix,
                         imgLR[points_postion[j]+251]-cent_pix)
          #pad leftside to zero
        }else if(points_postion[j] %in% rightside_index){
          neigh_pix <- c(imgLR[points_postion[j]-251]-cent_pix,
                         imgLR[points_postion[j]-250]-cent_pix,
                         0,
                         imgLR[points_postion[j]-1]-cent_pix,
                         0,
                         imgLR[points_postion[j]+249]-cent_pix,
                         imgLR[points_postion[j]+250]-cent_pix,
                         0)
          #pad rightside to zero
        }else if(points_postion[j]==1){
          neigh_pix <- c(0,
                         0,
                         0,
                         0,
                         imgLR[points_postion[j]+1]-cent_pix,
                         0,
                         imgLR[points_postion[j]+250]-cent_pix,
                         imgLR[points_postion[j]+251]-cent_pix)
        }else if(points_postion[j]==1){
          neigh_pix <- c(0,
                         0,
                         0,
                         0,
                         imgLR[points_postion[j]+1]-cent_pix,
                         0,
                         imgLR[points_postion[j]+250]-cent_pix,
                         imgLR[points_postion[j]+251]-cent_pix)
        }else if(points_postion[j]==250){
          neigh_pix <- c(0,
                         0,
                         0,
                         imgLR[points_postion[j]-1]-cent_pix,
                         0,
                         imgLR[points_postion[j]+249]-cent_pix,
                         imgLR[points_postion[j]+250]-cent_pix,
                         0)
        }else if(points_postion[j]==62251){
          neigh_pix <- c(0,
                         imgLR[points_postion[j]-250]-cent_pix,
                         imgLR[points_postion[j]-249]-cent_pix,
                         0,
                         imgLR[points_postion[j]+1]-cent_pix,
                         0,
                         0,
                         0)
        }else if(points_postion[j]==62500){
          neigh_pix <- c(imgLR[points_postion[j]-251]-cent_pix,
                         imgLR[points_postion[j]-250]-cent_pix,
                         0,
                         imgLR[points_postion[j]-1]-cent_pix,
                         0,
                         0,
                         0,
                         0)
        }
        
        featMat[j+1000*(n_files-1),,k]=neigh_pix
        
        
        multi_index <- points_postion[j] %% 250
        if (multi_index==0){multi_index=250}
        sub_pix <- c(imgHR[4*points_postion[j]-(2*multi_index+1)],
                     imgHR[4*points_postion[j]-(2*multi_index)],
                     imgHR[4*points_postion[j]+500-(2*multi_index+1)],
                     imgHR[4*points_postion[j]+500-(2*multi_index)])
        
        labMat[j+1000*(n_files-1),,k]= sub_pix
        
        
      } 
      
          
      
          ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      
      ### step 3. repeat above for three channels
        
    }
  }
  return(list(feature = featMat, label = labMat))
}

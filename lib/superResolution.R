########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3

superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    LR_nrow <- nrow(imgLR)
    LR_ncol <- ncol(imgLR)
    num_ele <- LR_nrow * LR_ncol
    featMat <- array(NA, c(num_ele, 8, 3))
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    ele_locations <- t(sapply(1 : num_ele, locate, LR_ncol))
    new_locations <- ele_locations + 1
    
    for (k in c(1:3)) {
      # supplementary image matrix
      supp_imgLR <- cbind(imgLR[, 1, k], imgLR[, , k], imgLR[, LR_ncol, k])
      supp_imgLR <- rbind(cbind(imgLR[1, 1, k], imgLR[1, , k],imgLR[1, LR_ncol, k]), 
                          supp_imgLR, cbind(imgLR[LR_nrow, 1, k], imgLR[LR_nrow, , k],imgLR[LR_nrow, LR_ncol, k]))
      
      ### fill the featM 
      # j = 1
      loc1 <- cbind(new_locations[, 1] - 1, new_locations[, 2] - 1)
      featMat[,  1, k] <- apply(loc1, 1, find, matrix = supp_imgLR)
      # j = 2
      loc2 <- cbind(new_locations[, 1] - 1, new_locations[, 2])
      featMat[,  2, k] <- apply(loc2, 1, find, matrix = supp_imgLR)
      # j = 3
      loc3 <- cbind(new_locations[, 1] - 1, new_locations[, 2] + 1)
      featMat[,  3, k] <- apply(loc3, 1, find, matrix = supp_imgLR)
      # j = 4
      loc4 <- cbind(new_locations[, 1], new_locations[, 2] - 1)
      featMat[,  4, k] <- apply(loc4, 1, find, matrix = supp_imgLR)
      # j = 5
      loc5 <- cbind(new_locations[, 1], new_locations[, 2] + 1)
      featMat[,  5, k] <- apply(loc5, 1, find, matrix = supp_imgLR)
      # j = 6
      loc6 <- cbind(new_locations[, 1] + 1, new_locations[, 2] - 1)
      featMat[,  6, k] <- apply(loc6, 1, find, matrix = supp_imgLR)
      # j = 7
      loc7 <- cbind(new_locations[, 1] + 1, new_locations[, 2])
      featMat[,  7, k] <- apply(loc7, 1, find, matrix = supp_imgLR)
      # j = 8
      loc8 <- cbind(new_locations[, 1] + 1, new_locations[, 2] + 1)
      featMat[,  8, k] <- apply(loc8, 1, find, matrix = supp_imgLR)
      #featMat[,  , k] <- t(apply(new_locations, 1, distribute1, img = supp_imgLR))
      #########
      #print(paste("k = ", k, sep = ""))
    }
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList = fit_train, featMat)
    
    ### step 3. recover high-resolution from predMat and save in HR_dir
    imgHR_fit <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    base_row <- seq(1, 2 * LR_nrow, 2)
    base_col <- seq(1, 2 * LR_ncol, 2)
    imgHR_fit[base_row, base_col, ] <- predMat[, 1, ]
    imgHR_fit[base_row, base_col + 1, ] <- predMat[, 2, ]
    imgHR_fit[base_row + 1, base_col, ] <- predMat[, 3, ]
    imgHR_fit[base_row + 1, base_col + 1, ] <- predMat[, 4, ]
    
  }
}


####test















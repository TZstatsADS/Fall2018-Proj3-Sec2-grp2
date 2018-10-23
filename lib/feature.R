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
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    ### step 1. sample n_points from imgLR
    
    ####Set up RGB channels matrices####
    #-Red channel-#
    img_red <- imgLR[,,1]
    #pad zero for boundary points
    a1 <- matrix(0,nrow=1,ncol=ncol(img_red))
    r1 <- rbind(img_red,a1)
    r2 <- rbind(a1,r1)
    a2 <- matrix(0,nrow=nrow(r2),ncol=1)
    r3 <- cbind(r2,a2)
    r4 <- cbind(a2,r3)
    
    img_red_h <- imgHR[,,1]
    s1 <- sample(1:length(img_red),n_points)
    
    #-Green channel-#
    img_green <- imgLR[,,2]
    #pad zero for boundary points
    a3 <- matrix(0,nrow=1,ncol=ncol(img_green))
    r5 <- rbind(img_green,a3)
    r6 <- rbind(a3,r5)
    a4 <- matrix(0,nrow=nrow(r6),ncol=1)
    r7 <- cbind(r6,a4)
    r8 <- cbind(a4,r7)
    
    img_green_h <- imgHR[,,2]
    s2 <- sample(1:length(img_green),n_points)
    
    #-Blue channel-#
    img_blue <- imgLR[,,3]
    #pad zero for boundary points
    a5 <- matrix(0,nrow=1,ncol=ncol(img_blue))
    r9 <- rbind(img_blue,a5)
    r10 <- rbind(a5,r9)
    a6 <- matrix(0,nrow=nrow(r10),ncol=1)
    r11 <- cbind(r10,a6)
    r12<- cbind(a6,r11)
    
    img_blue_h <- imgHR[,,3]
    s3 <- sample(1:length(img_blue),n_points)
    
    ### step 2. for each sampled point in imgLR,
    
        ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
        ###           tips: padding zeros for boundary points
    for (p in 1:n_points){
      #Red channel#
      c1 <- s1[p]+nrow(img_red)+2*(s1[p]%%ncol(img_red))-1
      
      #Upper left
      featMat[p,1,1] <- r4[c1-nrow(r4)-1]-r4[c1]
      #Upper center
      featMat[p,2,1] <- r4[c1-1]-r4[c1]
      #Upper right
      featMat[p,3,1] <- r4[c1+nrow(r4)-1]-r4[c1]
      #Center left
      featMat[p,4,1] <- r4[c1-nrow(r4)]-r4[c1]
      #Center right
      featMat[p,5,1] <- r4[c1+nrow(r4)]-r4[c1]
      #Lower left
      featMat[p,6,1] <- r4[c1-nrow(r4)+1]-r4[c1]
      #Lower center
      featMat[p,7,1] <- r4[c1+1]-r4[c1]
      #Lower right
      featMat[p,8,1] <- r4[c1+nrow(r4)+1]-r4[c1]
      
      
     
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      labMat[p,1,1] <- img_red_h[s1[p]*2-1+2*nrow(img_red)*(s1[p]%%ncol(img_red)-1)]
      labMat[p,2,1] <- img_red_h[s1[p]*2+2*nrow(img_red)*(s1[p]%%ncol(img_red)-1)]
      labMat[p,3,1] <- img_red_h[s1[p]*2-1+2*nrow(img_red)*(s1[p]%%ncol(img_red)-1)+2*nrow(img_red)]
      labMat[p,4,1] <- img_red_h[s1[p]*2-1+2*nrow(img_red)*(s1[p]%%ncol(img_red)-1)+2*nrow(img_red)+1]
      
      #Green channel#
      c2 <- s2[p]+nrow(img_green)+2*(s2[p]%%ncol(img_green))-1
      #Upper left
      featMat[p,1,2] <- r8[c2-nrow(r8)-1]-r8[c2]
      #Upper center
      featMat[p,2,2] <- r8[c2-1]-r8[c2]
      #Upper right
      featMat[p,3,2] <- r8[c2+nrow(r8)-1]-r8[c2]
      #Center left
      featMat[p,4,2] <- r8[c2-nrow(r8)]-r8[c2]
      #Center right
      featMat[p,5,2] <- r8[c2+nrow(r8)]-r8[c2]
      #Lower left
      featMat[p,6,2] <- r8[c2-nrow(r8)+1]-r8[c2]
      #Lower center
      featMat[p,7,2] <- r8[c2+1]-r8[c2]
      #Lower right
      featMat[p,8,2] <- r8[c2+nrow(r8)+1]-r8[c2]


      labMat[p,1,2] <- img_green_h[s2[p]*2-1+2*nrow(img_green)*(s2[p]%%ncol(img_green)-1)]
      labMat[p,2,2] <- img_green_h[s2[p]*2+2*nrow(img_green)*(s2[p]%%ncol(img_green)-1)]
      labMat[p,3,2] <- img_green_h[s2[p]*2-1+2*nrow(img_green)*(s2[p]%%ncol(img_green)-1)+2*nrow(img_green)]
      labMat[p,4,2] <- img_green_h[s2[p]*2-1+2*nrow(img_green)*(s2[p]%%ncol(img_green)-1)+2*nrow(img_green)+1]


       #Blue channel#
      c3 <- s3[p]+nrow(img_blue)+2*(s3[p]%%ncol(img_blue))-1
      #Upper left
      featMat[p,1,3] <- r12[c3-nrow(r12)-1]-r12[c3]
      #Upper center
      featMat[p,2,3] <- r12[c3-1]-r12[c3]
      #Upper right
      featMat[p,3,3] <- r12[c3+nrow(r12)-1]-r12[c3]
      #Center left
      featMat[p,4,3] <- r12[c3-nrow(r12)]-r12[c3]
      #Center right
      featMat[p,5,3] <- r12[c3+nrow(r12)]-r12[c3]
      #Lower left
      featMat[p,6,3] <- r12[c3-nrow(r12)+1]-r12[c3]
      #Lower center
      featMat[p,7,3] <- r12[c3+1]-r12[c3]
      #Lower right
      featMat[p,8,3] <- r12[c3+nrow(r12)+1]-r12[c3]

      labMat[p,1,3] <- img_blue_h[s3[p]*2-1+2*nrow(img_blue)*(s3[p]%%ncol(img_blue)-1)]
      labMat[p,2,3] <- img_blue_h[s3[p]*2+2*nrow(img_blue)*(s3[p]%%ncol(img_blue)-1)]
      labMat[p,3,3] <- img_blue_h[s3[p]*2-1+2*nrow(img_blue)*(s3[p]%%ncol(img_blue)-1)+2*nrow(img_blue)]
      labMat[p,4,3] <- img_blue_h[s3[p]*2-1+2*nrow(img_blue)*(s3[p]%%ncol(img_blue)-1)+2*nrow(img_blue)+1]
       
     # 
     # # ### step 3. repeat above for three channels
     # 
     # 
     # 
  }
  return(list(feature = featMat, label = labMat))
  }}




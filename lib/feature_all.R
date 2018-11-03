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
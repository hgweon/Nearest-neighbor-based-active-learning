
## The nearest neighbor based active leraning approach (NNAL)
## Input: initial labeled data DL, unlabeled data DU, 
##        parameter k, batch size h, number of sampling iterations nround
## Output: augmented labeled data

# Note:
# The function assumes that the first column of data is the class vector
# Currently, the function only uses the Euclidean distance.

NNAL <- function(DL, DU, h=10, k=5, nround=10)
{
    dist_out <- as.matrix(dist(as.matrix(DU[,-1])))
    diag(dist_out) <- max(dist_out)
    for(j in 1:nround)
    {
      y_tr1 <- DL[,1]
      x_tr1 <- DL[,-1]
      y_tr2 <- DU[,1]
      x_tr2 <- DU[,-1]
      
      n_class <- length(table(y_tr1))
      cname <- names(table(y_tr1))
      
      if(n_class==1)
      {
        for(i in 1:h)
        {
          temp <- x_tr1[which(y_tr1==cname),]
          dists <- as.matrix(pdist(x_tr2,temp))
          dmin1 <- as.numeric(apply(dists,1,min))   
          uncertainty <- rep(1,nrow(DU))
          ave_dist <- numeric(nrow(dist_out))
          for(l in 1:nrow(dist_out))
          {
            ave_dist[l] <- mean(sort(dist_out[l,])[2:(k+1)])
          } 
          if(i==1) 
          {
            utility <- (dmin1/ave_dist)
            idx <- order(uncertainty*utility,decreasing=TRUE)[1]
            dist_out2 <- cbind(dmin1,dist_out[idx,])        
            dist_out2 <- dist_out2[-idx,]
          }
          if(i>1) 
          {
            utility <- apply(dist_out2,1,min)/ave_dist
            idx <- order(uncertainty*utility,decreasing=TRUE)[1]
            dist_out2 <- cbind(dist_out2,dist_out[idx,])
            dist_out2 <- dist_out2[-idx,]
          }
          dist_out <- dist_out[-idx,-idx]
          DL_add <- DU[idx,]
          DL <- rbind(DL,DL_add)
          DU <- DU[-idx,]
        }
      }
      
      if(n_class>1)
      {
        for(i in 1:h)
        {
          if(i==1) 
          {
            ccnn <- matrix(0,nrow=nrow(DU),ncol=n_class)
            for(j in 1:n_class)
            {
              temp <- x_tr1[which(y_tr1==cname[j]),]
              dists <- as.matrix(pdist(x_tr2,temp))
              ccnn[,j] <- apply(dists,1,min)    
            }
            dmin1 <- apply(ccnn, 1, min)
            dmin2 <- apply(ccnn, 1, function(x) sort(x)[2]) 
            uncertainty <- (dmin1/dmin2)
          }
          ave_dist <- numeric(nrow(dist_out))
          for(l in 1:nrow(dist_out))
          {
            ave_dist[l] <- mean(sort(dist_out[l,])[2:(k+1)])
          } 
          if(i==1) 
          {
            utility <- (dmin1/ave_dist)
            idx <- order(uncertainty*utility,decreasing=TRUE)[1]
            dist_out2 <- cbind(dmin1,dist_out[idx,])        
            dist_out2 <- dist_out2[-idx,]
          }
          if(i>1) 
          {
            utility <- apply(dist_out2,1,min)/ave_dist
            idx <- order(uncertainty*utility,decreasing=TRUE)[1]
            dist_out2 <- cbind(dist_out2,dist_out[idx,])
            dist_out2 <- dist_out2[-idx,]
          }
          uncertainty <- uncertainty[-idx]
          dist_out <- dist_out[-idx,-idx]
          DL_add <- DU[idx,]
          DL <- rbind(DL,DL_add)
          DU <- DU[-idx,]
          ccnn <- ccnn[-idx,]
        }
      }        
    }
    list(DL,DU)
}




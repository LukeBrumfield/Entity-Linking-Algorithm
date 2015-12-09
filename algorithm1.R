
#Input:
#  G = (V, E) /*followee-follower network*/
#  H /*maximum number of hops*/
#  e = (u,v) followee-follower (u follows v)
algorithm1 <- function(G, H){

V <- data.frame(G[1])
E <- data.frame(G[2])
size <- length(V[,1])
V[2] <- 0
R <- matrix(nrow = size, ncol = size, data = 0)

colnames(R) <- as.character(V[,1])
rownames(R) <- as.character(V[,1])

V[2] <- 0

for (e in 1:nrow(E)){
  
  R[as.character(E[e,1]),as.character(E[e,2])] <- 1  
}

for (len in 2:H){
  #for each u in V
  for (u in V[[1]])
  {
    u <- as.character(u)
    T <- which(R[u,] == 1)#followees of u    
    
    V[,2] <- 0 #nv and Vt for each u

      for (t in T)
      {
        t <- as.character(t)
        if(t %in% rownames(R))
        {
          Vt <- which(R[,t] != 0) #/*reachable from t in len-1 hops*/
          for (i in Vt)
          {
            V[i,2] <- V[i,2] + 1
          }
        }
      }    
    
      for (v in V[[1]])
      {
        v <- as.character(v)
        if (R[u,v] == 0)
        {
          R[u,v] <- (1/len)*(V[v,2]/length(T))
          
          if (is.na(R[u,v]))
          {
            R[u,v] <- 0
          }
        }
      }
    
  }
}
return(R)
}
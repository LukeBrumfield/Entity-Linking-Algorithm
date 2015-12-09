algorithm2 <- function(G, H){
  
  V <- data.frame(G[1])
  E <- data.frame(G[2])
  colnames(E) <- c("u","v")
  colnames(V) <- c("V")
  hops <- c()
  
  
  size <- length(V[[1]])
  
  Lin <- list()
  count <- 0
 
  for (v in V[,1])
  {
    Z <- E[which(E[,2] == v),1]    
    for(z in Z)
    {    
      Lin[[as.character(v)]][[as.character(z)]] <- list(c(z,1))
    }
  }
 
  Lout <- list()
  count <- 0
 
 for (v in V[,1])
 {
   Z <- E[which(E[,1] == v),2]
   for(z in Z)
   {
     if(count == 0)
     {
       Lout[[as.character(v)]][[as.character(z)]] <- list(c(z,1,list(E[which(E[,1] == z),2])))
       count <- 1
     }else
     {
       Lout[[as.character(v)]][[as.character(z)]] <- c(Lout[[as.character(v)]][[as.character(z)]],list(c(z,1,list(E[which(E[,1] == z),2]))))
     }
   }
 }
  
  Q <- list()
 
  for (k in 1:size)
  {
    Vk <- V[k,]
    
    Q <- list(c(Vk, 0))
        
    while(length(Q) != 0)
    {
      item <- Q[[1]]
      Q <- Q[-1]      
      
      len <- item[[2]] + 1
      
      Nin <- c()
      
      for(x in Lin[[as.character(Vk)]])
      {
        Nin <- c(Nin, x[[1]][1])
      }
      
      for(s in Nin)
      {
        LkOuts <- Lout[[as.character(Vk)]][[as.character(s)]][[1]][[3]]
        LkIns <- c()
        
        for (x in Lin[[as.character(Vk)]])
        {
          LkIns <- c(LkIns, x[[1]][1])
        }
        
        dsvk <- length(intersect(LkIns, LkOuts))
        
        if (len < dsvk)#11
        {
          hops <- c(hops, s , Vk)
          if(length(LkOuts) > 0)#13
          {
            Lout[[as.character(Vk)]][[as.character(s)]][[3]] <- NULL #14
          }
          Lout[[as.character(Vk)]][[as.character(s)]][[3]] <- list(Vk, len, Nin)
          
          if(len < H)
          {
            if (length(Q) == 0)
            {
              Q <- c(Q, list(c(s,len)))
            }
            else
            {
              vals <-c()
              for(q in Q)
              {
                vals <- c(vals, q[[1]][1])
              }
              if (!(s %in% vals))
              {
                Q <- c(Q, list(c(s,len)))
              }
            }
          }
        }
        else if(len == dsvk)
        {
          hops <- c(hops, s , Vk)
          if(length(LkOuts) > 0)
          {            
            Lout[[as.character(Vk)]][[as.character(s)]][[3]] <- union(LkOuts,Nin) #14
          }
          else
          {
            Lout[[as.character(Vk)]][[as.character(s)]][[3]] <- list(Vk, len, Nin)
          }
        }
      }
    }
  }
 
 return(list(Lin,Lout,hops)) 
}
source('~/Twitter/algorithm1.R')
source('~/Twitter/algorithm2.R')

u <- sample(x = 1:100, size = 750, replace = TRUE)
v <- sample(x = 1:75, size = 750, replace = TRUE)
times <- sample(x = 1:30, size = 750, replace = TRUE)

D <- data.frame(u,v,times)
E <- data.frame(u,v)
V <- sort(unique(c(E[[1]], E[[2]])))
G <- list(V, E)
H <- 2
R <- algorithm1(G,H)
write.csv(R, "results.csv")
L <- algorithm2(G,H)
Lin <- L[[1]]
Lout <- L[[2]]
hops <- unique(L[[3]])

alpha <- 0.6
beta <- 0.4
gamma <- 0.1
tau <- 7
now <- 30

S <- list()

for(v in V)
{  
  Sin <- sum(R[v,])/sum(R[v,] != 0)
  Sp <- nrow(E[E$v == 1,])/nrow(E)
  Sr <- length(intersect(which(D[,3] > now - tau), which(D[,2] == v)))/nrow(D)
  Se <- Sin * alpha + Sp * beta + Sr * gamma
  S <- c(S, list(c(as.integer(v),Sin,Sp,Sr,Se)))
}

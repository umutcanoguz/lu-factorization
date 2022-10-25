A <- matrix(c(1,1,0,3,
              2,1,-1,1,
              3,-1,-1,2,
              -1,2,3,-1), nrow = 4, ncol = 4, byrow = T)
A
LU <- function(A)
{
  n <- nrow(A)
  L <- U <-  matrix(c(0),nrow = n, ncol = n)
  diag(L) <- 1
  k <- 1
  
  #step 1
  U[1,1] <- A[1,1]/L[1,1]
  if(U[1,1] * L[1,1]==0)
  {
    stop("matris çarpanları ayırmak mümkün değil ")
  }
  L;U
  #step 2
  for(j in 2 : n)
  {
    U[1,j] <- A[1,j]/L[1,1]
    L[j,1] <- A[j,1]/U[1,1]
  }
  L;U
  #step3
  for(i in 2 :(n-1))
  {
    L;U
    
    #step4
    U[i,i] <- (A[i,i]-sum(L[i,k:(i-1)]*U[k:(i-1),i]))/L[i,i]
    if(L[i,i]*U[i,i]==0)
    {
      stop("çarpanları ayırmak imkansız")
    }
    L;U
    #step5
    for(j in (i+1) :n)
    {
      U[i,j] <- (1/L[i,i])* (A[i,j]-sum(L[i,k:(i-1)]*U[k:(i-1),j]))
      #L[j,i] <- (1/U[i,i])* (A[j,i]-sum(L[j,k:(i-1)]*U[k:(i-1),i]))
    }
    L;U
      
    
  }
  #step6
  U[n,n] <- (A[n,n]-sum(L[n,1:(n-1)]*U[1:(n-1),n]))/L[n,n]
  if(L[n,n]*U[n,n]==0)
  {
    cat("çözüm tekdir")
  }
  L;U
  
  
  
  
  
    
}
L
U


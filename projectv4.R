rm(list=ls())

#Function V_n of V0, mu, sigma, acc to problem description.

portfolio_n_return <- function(mu,v0,sigma,n){
  product = 1
  x <-rnorm(n)
  y <- rnorm(n)
  returnproduct = for(i in 1:n){
    stockreturn <- exp(mu+sigma*x[i])
    dividend <- 0.05*exp(-(0.05^2)/2+0.05*y[i])
    product = product * (stockreturn + dividend)
  }
  Vn <- v0*product
}

#vector to test different n:s
n <- c(1:50)
nvector <- c(1:50)

#start values

v0 <- 10^6
mu <- 0.1
sigma <- 0.1
number_of_sims <- 10000000


#fuck it

#capvec<-c()
#retvec<-c()

#start a dataframe to store the values, columns are different n:s, rows are different simulations = Vn for the specific n.

simulation <-setNames(data.frame(matrix(ncol = length(nvector), nrow = number_of_sims)), nvector)

for (x in nvector){
  
  for (i in 1:number_of_sims){
    
    simulation[i,x] <- portfolio_n_return(mu,v0,sigma,x)
  }
}

#plot(density(simulation[,50]))

losses <- simulation

#R0<-exp(0.02*n)

for (x in nvector){
  
  for (i in 1:number_of_sims){
    R0 <- exp(0.02*x)
    losses[i,x] <- losses[i,x]-R0*v0
  }
}


#varvec stores n different values for var
varvec<-c()


quant<-0.05 #f?r att v?lja vilket nedre intervall

for (i in n){
  varvec[i] <- quantile(losses[,i],quant)
}

for (i in n){
  
  for (j in 1:200){
    vn<-portfolio_n_return(mu,v0,sigma,n)
    capvec[j]<-vn
    retvec[j]<-vn/v0
    
  }
  varvec[i]<-quantile(capvec-R0*v0, quant)
} #genererar en vektor Var fr?n 200 observations samples

#Svaret på fråga b... Plot the empirical distribution of Vn.
plot(density(capvec), xlab="Vn", ylab="nope", main="Simulated Vn, for n=8")



plot(density(varvec))
#plot(hist(var))
qqnorm(varvec)
qqline(varvec, col="red")



# part d
capvec2<-c()
varvec2<-c()



for (i in 1:number_of_sims){
  
  for (j in 1:200){
    depression<-runif(1)
    
    if(depression<0.05){
      vn<-portfolio_n_return(0.5*mu,v0,1.5*sigma,n)
      capvec2[j]<-vn
    } else {
      vn<-portfolio_n_return(mu,v0,sigma,n)
      capvec2[j]<-vn
    }
    
  }
  varvec2[i]<-quantile(capvec2-R0*v0, quant)
  
}

plot(density(varvec2))
qqnorm(varvec2)
qqline(varvec2, col="red")

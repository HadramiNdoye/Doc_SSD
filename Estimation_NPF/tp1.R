# Exercise 1 
landa <- 0.5
n <- 100
L <- rexp(n,landa)
x <- seq(1,100,1)
F.repartition <- 1 - exp(-landa * x)
F.repartition.emperique <- plot(ecdf(L),col="blue",
                                main="fonction de repartition")
lines(x,F.repartition,col="red")
# checking the normality
xfix <- 2
Fn <- ecdf(L)
n <- 100
m <- 1000
ech <- replicate(m,rexp(n,landa))
Fnx <- apply(ech,1,Fn)
hist((sqrt(n) * (Fnx - pexp(2,landa))) / sqrt(Fnx * (1 - Fnx)),probability = TRUE)
curve(dnorm(x,0,1),add=TRUE,col="red")

# Confidence interval

supmax <- function(n){
  sup <- c()
    for(j in 1:length(x)-1){
      sup[j] <- max(((1 - j)/ n)+pexp(x[j],landa),-pexp(x[j+1],landa)+(j+1)/n)
    }
  return (sup)
}
max_fnfx <- supmax(10000)
plot(1:(n-1),max_fnfx,col="red",xlab = "n")
alpha <- 0.05
born_inf <- Fnx - ((qexp(1-(alpha/2),landa)) / sqrt(n)) * sqrt(Fnx * (1-Fnx))
Fnx
born_sup <- Fnx + ((qexp(1-(alpha/2),landa)) / sqrt(n)) * sqrt(Fnx * (1-Fnx))

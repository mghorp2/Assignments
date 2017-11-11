
p <- seq(0, 1, 0.001)
gini.index <- 2 * p * (1 - p)
class.error <- 1 - pmax(p, 1 - p)
entropy <- - (p * log(p) + (1 - p) * log(1 - p))
plot(range(p),range(cbind(gini.index, class.error, entropy)),type='n',xlim = c(0,1),ylim = c(0,1),ylab = "values of Gini Index, Classification Error, Entropy",xlab = "Pm")
lines(p,gini.index,type='l',col="Red")
lines(p,class.error,type='l',col="Blue")
lines(p,entropy,type='l',col="Green")

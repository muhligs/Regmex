rw.ss <-
function(fg.freq,bg.freq=0.05){#include
n <- log(fg.freq/bg.freq,10)	
d <- log((1-fg.freq)/(1-bg.freq),10)
res <- n*(-1/d)
ss <- 	c(-1,floor(res))
out <- c(0.95,rep(0,ss[2]),0.05)
	names(out) <- c(-1,0,(1:ss[2]))
sf <- stepfun(x = c(bg.freq),y = c(ss[2],-1))
return(list(score.scheme = out, score.function = sf))
}

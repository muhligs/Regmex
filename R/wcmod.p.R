wcmod.p <-
function(p,n){ #include
l<--log(1-p)
c.l<-cumsum(l)
r<-(c(0,c.l[-length(c.l)])+c.l)/2	
	n.t<-sum(n)
	l.t=sum(l)
	T<-sqrt(sum(n))*((sum(n*r))/n.t-l.t/2)/l.t
return(2*pnorm(-abs(T),mean=0,sd=sqrt(1/12)))
}

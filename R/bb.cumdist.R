bb.cumdist <-
function(m,n,varls){return(ifelse(m<0.01,1,abs(2*sum((-1)^(0:999)*exp(-2/n/varls*((1:1000)^2)*m^2)))))}

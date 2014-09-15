bb.cumdist <-
function(m,n){return(ifelse(m<0.01,0,abs(1-2*sum((-1)^(0:999)*exp(-2/n*((1:1000)^2)*m^2)))))}

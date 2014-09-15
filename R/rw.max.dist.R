rw.max.dist <-
function(dist, n.exc) { # include
#return(1 - (1-dist)^n.exc)
return(ifelse(dist>0.0000001,1 - (1-dist)^n.exc,dist*n.exc)) # because of underflow we use a bernoulli inequality
}

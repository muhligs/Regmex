rw.walk2 <-
function (scores) #include
{
    cs <- cumsum(scores)
    neg.cs <- cs < 0
    while (sum(neg.cs) > 0) {
        cs[which(neg.cs)[1]:length(cs)] <- cs[which(neg.cs)[1]:length(cs)] +  1
        neg.cs <- cs < 0
    }
    return(cs)
}

all.mers <-
function(mer){sort(apply(expand.grid(lapply(1:mer,function(x){c("A","C","G","T")})),1,function(x)paste(x,collapse="")))}

rev.comp <-
function(x){paste(chartr("CGAT","GCTA",rev(unlist(strsplit(x,"")))),collapse="")}

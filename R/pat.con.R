pat.con <-
function(pattern, overlap = TRUE){ # include
   java.pat<-reg.exp.conv(pattern)
  #system(paste("java -jar /home/mmn/Dropbox/motiv_analysis/automata/automata.jar", java.pat, "patMatNonOver", "patMatOver"))
	pattern1<-gsub("\\(","1",pattern)
	pattern1<-gsub("\\)","2",pattern1)
	pattern1<-gsub("\\|","3",pattern1)
	pattern1<-gsub("\\*","4",pattern1)
	 system(paste("java -jar ",file.path(system.file(package="Regmex"),"exec/automata.jar "), java.pat, " ",pattern1,"patMatNonOver ", pattern1,"patMatOver",sep=""))
	if(!overlap){
	  mat <- read.table(paste(pattern1,"patMatNonOver",sep=""),skip=2)+1
  lines <- readLines(paste(pattern1,"patMatNonOver",sep=""),2)
  } else {
  mat <- read.table(paste(pattern1,"patMatOver",sep=""),skip=2)+1
  lines <- readLines(paste(pattern1,"patMatOver",sep=""),2)
  }
  rownames(mat)<-1:dim(mat)[1]
  start <- as.numeric(lines[1])+1; end <- as.numeric(unlist(strsplit(lines[2]," ")))+1
  lst<-list("pattern"=pattern,"matrix"=as.matrix(mat),"startState"=start,"endState"=end,"matrix.di"=dfa.di(as.matrix(mat)))
  system(paste("rm ",pattern1,"patMatOver ",pattern1,"patMatNonOver",sep=""))
   class(lst)<-"pattern"
	 return(lst)
}

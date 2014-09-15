reg.exp.conv <-
function(pat2){ # include
   pat2<-gsub("(\\[[GCAT])","\\1|",pat2)
   count=0
   while(grepl("[GCAT*]\\]",pat2)&count<200){pat2<-gsub("(\\|[GCAT*])","\\1|",pat2);count<-count+1}
   count=0
   while(grepl("\\|\\|",pat2)&count<1000){pat2<-gsub("\\|\\|","|",pat2);count=count+1}
   pat2<-gsub("\\[","(",pat2)
   pat2<-gsub("\\|\\]",")",pat2)
   pat2<-gsub("\\|\\*","*",pat2)
   pat2<-sub("$","'",pat2)
   pat2<-sub("^","'",pat2)
   pat2
}

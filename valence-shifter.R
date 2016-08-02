#!/usr/bin/Rscript

#======================================================================#
# 
# Description: An R wrapper to: 
# 	1) Create a valence shifting variable from a CoNLL corpus.
#	2) Save a lemmatized corpus for the creation of a vector space model. 
# 
# Usage: 
# Rscript valence-shifter.R [input-dir] [path/looper.so] [formatted-corpus-output]
#
# where
# [input-dir] is the directory containing the input CoNLL files
# [path/looper.so] is the path to the shared object looper.so.  
# [formatted-corpus-output] is the directory and file name for the lemmatized corpus.
#
# Author: L. Rheault 
# 
#======================================================================#

library(data.table)
																		
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#

args <- commandArgs(trailingOnly = TRUE)
lofiles <- list.files(args[1],full.names=TRUE)
n <- length(lofiles)

# Loading C program
dyn.load(args[2])
looper <- function(x,y,z,m) {
  newx <- .C("looper", x=as.double(x),y=as.character(y),z=as.double(z),m=as.integer(m))
  newx[["x"]]
}
# List of negation words
negw <- c("never","no","nothing","nowhere","none","not","cannot")
q <- length(negw)

for (i in 1:n){
	temp <- fread(lofiles[i])
	m <- nrow(temp)
	### Add valence shifting variable:
	temp$valence <- 0
	for (j in 1:q){
    	temp$valence[temp$lemma==negw[j]] <- 1
  	}
  	temp$valence <- looper(temp$valence, temp$lemma, temp$s, m)
	write.table(temp,lofiles[i],row.names=FALSE,col.names=TRUE,quote=FALSE,append=FALSE)
	### Appending sentences in lemmatized form:
	ls <- temp[,list(speech = paste(lemma,pos1,sep="_",collapse=" ")),by=l]$speech
	write(ls,args[3],append=TRUE)

	rm(temp)
	rm(ls)
 	cat(format(Sys.time(), "%X"),"\n")
  	cat("File",i,"of",n,"has been processed.","\n")
}

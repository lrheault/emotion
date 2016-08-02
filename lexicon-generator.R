#!/usr/bin/Rscript

#======================================================================#
# 
# Description: An R script to create a polarity lexicon based on a vector space model and seed words. 
# 
# Usage: 
# Rscript lexicon-generator.R [vectorfile] [path-to-aux-files] [lower-bound] [lexicon-size]  
#
# where
# [vectorfile] = path and name of vector file
# [path-to-aux-files] = the directory containing all other input files and dependencies, also the output directory.
# [lower-bound] = only consider words occurring more than this bound (0 to use all words)  
# [lexicon-size] = how many words should the lexicon contain (e.g. 1000)	
#
# Dependencies:
# ---Vectors from vector space model;
# ---Vocabulary file ("vocab-merged.txt");
# ---nationalities auxiliary file  ("nationalities-unique.csv");
# ---seed files (positive and negative, "pos-100-final" and "neg-100-final").
#
# Output: A positive and a negative lexicon.
#
# Author: L. Rheault 
# 
#======================================================================#

library(data.table)
																		
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#

argv=commandArgs(TRUE)
vectorfile <- argv[1]
inpath <- argv[2]
hh <- as.numeric(argv[3])
lexsize <- as.numeric(argv[4])

#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Loading and Cleaning
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#

# loading vector space model (arg 1):
vin <- fread(vectorfile) 
vecdim <- ncol(vin)-1
# vocabulary file (arg 2):
dd <- fread(paste(inpath,"/vocab-merged.txt",sep=""))
ndd <- nrow(dd)
# keeping only words with (arg 3) or more occurrences:
x <- merge(vin,dd,by='V1')
x <- x[x$V2.y>=hh,]

# removing nationalities:
nat <- fread(paste(inpath,"/nationalities-unique.csv",sep=""))
nnat <- nrow(nat)
for (jj in 1:nnat){x <- x[grepl(nat[jj],x$V1)==FALSE,]}
# removing proper nouns and digits:
x <- x[grepl('[[:upper:]]',x$V1,perl=T)==FALSE,]
x <- x[grepl('[[:digit:]]',x$V1,perl=T)==FALSE,]
# keeping only adj, adv, nouns, verbs, and excalamations:
x <- x[grepl('_n|_a|_r|_v|_u',x$V1)==TRUE,]
# removing incorrectly pos-tagged 'formaltitle' instances:
x <- x[grepl('formaltitle',x$V1)==FALSE,]

# creating matrix and preserving dictionary:
y <- x[,2:vecdim+1,with=FALSE]
y <- as.matrix(y)
dictionary <- x[,1,with=FALSE]
rm(x)
rm(vin)

# loading seeds:
lexicon.pos <- readLines(paste(inpath,"/pos-100-final",sep=""))
lexicon.neg <- readLines(paste(inpath,"/neg-100-final",sep=""))
m <- length(lexicon.pos)
pos.index <- vector(length=m)
neg.index <- vector(length=m)
for (i in 1:m){  
  pos.index[i] <- which(dictionary$V1==lexicon.pos[i])
  neg.index[i] <- which(dictionary$V1==lexicon.neg[i])
}
pos.matrix <- y[pos.index,]
neg.matrix <- y[neg.index,]
# removing seeds from main lexicon:
seeds <- c(pos.index,neg.index)
y.noseed <- y[-seeds,]
dictionary.noseed <- dictionary[-seeds]

cat("Pre-processing stage successful.","\n")
cat(format(Sys.time(), "%X"),"\n")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Creating Lexicon
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#

# computing sentiment orientation:
q <- nrow(y.noseed)
vecpos <- vector(length=m)
vecneg <- vector(length=m)
results <- vector(length=q)
sumvecpos <- ""
sumvecneg <- ""
cat("Computing the lexicons...","\n")
for (i in 1:q){
  for (j in 1:m){
    vecpos[j] <- sum(y.noseed[i,]*pos.matrix[j,])/sqrt(sum(y.noseed[i,]^2)*sum(pos.matrix[j,]^2))
    vecneg[j] <- sum(y.noseed[i,]*neg.matrix[j,])/sqrt(sum(y.noseed[i,]^2)*sum(neg.matrix[j,]^2))  
  }
  sumvecpos <- sum(vecpos)
  sumvecneg <- sum(vecneg)
  results[i] <- sumvecpos - sumvecneg

  if (i %% 1000 == 0) {
    prog <- round(i/q*100, digits = 2)    
    cat("Progress:",prog,"%.","\n")
    cat(format(Sys.time(), "%X"),"\n")
  } 
}
res <- cbind(dictionary.noseed,results)
# rescaling between -1 and 1:
rescale <- function(x) {2/(max(x) - min(x))*(x - min(x)) - 1}
res$results <- rescale(res$results)

# creating positive and negative lexicons:
posres <- res[order(-results),]
negres <- res[order(results),]
exp.lex.pos <- posres[1:lexsize,]
exp.lex.neg <- negres[1:lexsize,]
write.table(exp.lex.pos,paste(inpath,"/positive-lexicon-",vecdim,"d-",lexsize,sep=""),sep=",",row.names=F,col.names=F)
write.table(exp.lex.neg,paste(inpath,"/negative-lexicon-",vecdim,"d-",lexsize,sep=""),sep=",",row.names=F,col.names=F)
# saving full lexicon (comment in to activate):
# res <- res[order(results),]
# write.table(res,paste(inpath,"/full-lexicon",vecdim,"d-",lexsize,sep=""),sep=",",row.names=F,col.names=F)

cat("Process complete and successful.","\n")
cat(format(Sys.time(), "%X"),"\n")

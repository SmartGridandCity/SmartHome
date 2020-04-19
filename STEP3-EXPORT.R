############################################ SAVE SEQUENCE FOR sequence prediction
#one sequence per line
pred <- data.frame(Sequence = character(), stringsAsFactors = FALSE)
temp <- seqen$idseq[1]
stringseq <- as.character(seqen$nclustkmeans[1])

for(i in 1:nrow(seqen))
{
  if(temp==seqen$idseq[i])
  {
    stringseq <- paste(stringseq,"-1", as.character(seqen$nclustkmeans[i]))
  }else{
    stringseq <- paste(stringseq,"-1 -2")
  pred <- rbind(pred, stringseq, stringsAsFactors = FALSE)
  temp <- seqen$idseq[i]
  stringseq <- as.character(seqen$nclustkmeans[i])
  }
}

write.table(pred, "output.csv", row.names=FALSE, col.names=FALSE)

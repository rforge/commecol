print.treeNodf.test<- function (x, digits=4, ...){


permutations<-x$permutations
treeNODF.rows<-x$treeNODF.rows
treeNODF.cols<-x$treeNODF.cols

if(is.na(treeNODF.rows)==FALSE){
   
       NODF.rows<-x$NODF.rows
    difNODF.rows<-x$difNODF.rows

    treeNODF.rows.aleats<-x$treeNODF.rows.aleats
        NODF.rows.aleats<-x$NODF.rows.aleats
     difNODF.rows.aleats<-x$difNODF.rows.aleats


   treeNODF.rows.aleats.mean<-mean(treeNODF.rows.aleats)
       NODF.rows.aleats.mean<-mean(    NODF.rows.aleats)
    difNODF.rows.aleats.mean<-mean( difNODF.rows.aleats)
   
   treeNODF.rows.aleats.sd<-sd(treeNODF.rows.aleats)
       NODF.rows.aleats.sd<-sd(    NODF.rows.aleats)
    difNODF.rows.aleats.sd<-sd( difNODF.rows.aleats)

   treeNODF.rows.cases<-sum(treeNODF.rows.aleats >= treeNODF.rows)
       NODF.rows.cases<-sum(    NODF.rows.aleats >=     NODF.rows)
    difNODF.rows.cases<-sum( difNODF.rows.aleats >=  difNODF.rows)

   treeNODF.rows.prob<-(treeNODF.rows.cases+1)/(permutations+1)
       NODF.rows.prob<-(    NODF.rows.cases+1)/(permutations+1)
    difNODF.rows.prob<-( difNODF.rows.cases+1)/(permutations+1)


   resu.rows<-as.data.frame(matrix(NA, 3, 5))
   resu.rows[,1]<-c(treeNODF.rows,             NODF.rows,                difNODF.rows)
   resu.rows[,2]<-c(treeNODF.rows.aleats.mean, NODF.rows.aleats.mean,    difNODF.rows.aleats.mean)
   resu.rows[,3]<-c(treeNODF.rows.aleats.sd,   NODF.rows.aleats.sd,      difNODF.rows.aleats.sd)
   resu.rows[,4]<-c( (resu.rows[,1]-resu.rows[,2]) /  resu.rows[,3] )
   resu.rows[,5]<-c(treeNODF.rows.prob,        NODF.rows.prob,           difNODF.rows.prob)
   
   rownames(resu.rows)<-c("treeNODF.rows","NODF.rows","difNODF.rows")
   colnames(resu.rows)<-c("Obs","M.aleat","SD.aleat","Z","Prob")
   }


if(is.na(treeNODF.cols)==FALSE){
  
       NODF.cols<-x$NODF.cols
    difNODF.cols<-x$difNODF.cols

   treeNODF.cols.aleats<-x$treeNODF.cols.aleats
       NODF.cols.aleats<-x$NODF.cols.aleats
    difNODF.cols.aleats<-x$difNODF.cols.aleats

   treeNODF.cols.aleats.mean<-mean(treeNODF.cols.aleats)
       NODF.cols.aleats.mean<-mean(    NODF.cols.aleats)
    difNODF.cols.aleats.mean<-mean( difNODF.cols.aleats)
   
   treeNODF.cols.aleats.sd<-sd(treeNODF.cols.aleats)
       NODF.cols.aleats.sd<-sd(    NODF.cols.aleats)
    difNODF.cols.aleats.sd<-sd( difNODF.cols.aleats)

   treeNODF.cols.cases<-sum(treeNODF.cols.aleats >= treeNODF.cols)
       NODF.cols.cases<-sum(    NODF.cols.aleats >=     NODF.cols)
    difNODF.cols.cases<-sum( difNODF.cols.aleats >=  difNODF.cols)

   treeNODF.cols.prob<-(treeNODF.cols.cases+1)/(permutations+1)
       NODF.cols.prob<-(    NODF.cols.cases+1)/(permutations+1)
    difNODF.cols.prob<-( difNODF.cols.cases+1)/(permutations+1)

   resu.cols<-as.data.frame(matrix(NA, 3, 5))
   resu.cols[,1]<-c(treeNODF.cols,             NODF.cols,                difNODF.cols)
   resu.cols[,2]<-c(treeNODF.cols.aleats.mean, NODF.cols.aleats.mean,    difNODF.cols.aleats.mean)
   resu.cols[,3]<-c(treeNODF.cols.aleats.sd,   NODF.cols.aleats.sd,      difNODF.cols.aleats.sd)
   resu.cols[,4]<-c( (resu.cols[,1]-resu.cols[,2]) /  resu.cols[,3] )
   resu.cols[,5]<-c(treeNODF.cols.prob,        NODF.cols.prob,           difNODF.cols.prob)

   rownames(resu.cols)<-c("treeNODF.cols","NODF.cols","difNODF.cols")
   colnames(resu.cols)<-c("Obs","M.aleat","SD.aleat","Z","Prob")
   }


if(is.na(treeNODF.rows)==FALSE & is.na(treeNODF.cols)==FALSE) {
   
   treeNODF.mat<-x$treeNODF.mat
       NODF.mat<-x$NODF.mat
    difNODF.mat<-x$difNODF.mat

   treeNODF.mat.aleats<-x$treeNODF.mat.aleats
       NODF.mat.aleats<-x$NODF.mat.aleats
    difNODF.mat.aleats<-x$difNODF.mat.aleats

   treeNODF.mat.aleats.mean<-mean(treeNODF.mat.aleats)
       NODF.mat.aleats.mean<-mean(    NODF.mat.aleats)
    difNODF.mat.aleats.mean<-mean( difNODF.mat.aleats)
   
   treeNODF.mat.aleats.sd<-sd(treeNODF.mat.aleats)
       NODF.mat.aleats.sd<-sd(    NODF.mat.aleats)
    difNODF.mat.aleats.sd<-sd( difNODF.mat.aleats)

   treeNODF.mat.cases<-sum(treeNODF.mat.aleats >= treeNODF.mat)
       NODF.mat.cases<-sum(    NODF.mat.aleats >=     NODF.mat)
    difNODF.mat.cases<-sum( difNODF.mat.aleats >=  difNODF.mat)

   treeNODF.mat.prob<-(treeNODF.mat.cases+1)/(permutations+1)
       NODF.mat.prob<-(    NODF.mat.cases+1)/(permutations+1)
    difNODF.mat.prob<-( difNODF.mat.cases+1)/(permutations+1)

   resu.mat<-as.data.frame(matrix(NA, 3, 5))
   resu.mat[,1]<-c(treeNODF.mat,             NODF.mat,                difNODF.mat)
   resu.mat[,2]<-c(treeNODF.mat.aleats.mean, NODF.mat.aleats.mean,    difNODF.mat.aleats.mean)
   resu.mat[,3]<-c(treeNODF.mat.aleats.sd,   NODF.mat.aleats.sd,      difNODF.mat.aleats.sd)
   resu.mat[,4]<-c( (resu.mat[,1]-resu.mat[,2]) /  resu.mat[,3] )
   resu.mat[,5]<-c(treeNODF.mat.prob,        NODF.mat.prob,           difNODF.mat.prob)

   rownames(resu.mat)<-c("treeNODF.mat","NODF.mat","difNODF.mat")
   colnames(resu.mat)<-c("Obs","M.aleat","SD.aleat","Z","Prob")
   }

if(is.na(treeNODF.rows)==FALSE & is.na(treeNODF.cols)==TRUE){print(resu.rows)}
if(is.na(treeNODF.rows)==TRUE  & is.na(treeNODF.cols)==FALSE){print(resu.cols)}
if(is.na(treeNODF.rows)==FALSE & is.na(treeNODF.cols)==FALSE){print(list(resu.rows=resu.rows,
                                                resu.cols=resu.cols, resu.mat=resu.mat))}


invisible(x)
}


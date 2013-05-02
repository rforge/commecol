treeNodf.test<-function(comm, col.tree, order.rows=FALSE, 
                               row.tree, order.cols=FALSE, 
                               null.model="perm.rows", permutations=999){
models<-c("perm.rows", "perm.cols", "perm.rc", 
          "perm.tip.rows", "perm.tip.cols", "perm.tip.rc", "ff")
if (is.na(pmatch(null.model, models))) {stop("You should specify a null model.")}


sites<-nrow(comm)
spp<-ncol(comm)

resu<-list(
         treeNODF.rows=NA,
             NODF.rows=NA,
          difNODF.rows=NA,

         treeNODF.cols=NA,
             NODF.cols=NA,
          difNODF.cols=NA,

         treeNODF.mat=NA,
             NODF.mat=NA,
          difNODF.mat=NA,

         treeNODF.rows.pairs=NA,
         treeNODF.cols.pairs=NA,
         
         treeNODF.rows.aleats=NA,
             NODF.rows.aleats=NA,
	  difNODF.rows.aleats=NA,

         treeNODF.cols.aleats=NA,
             NODF.cols.aleats=NA,
          difNODF.cols.aleats=NA,
         
         treeNODF.mat.aleats=NA,
             NODF.mat.aleats=NA,
	  difNODF.mat.aleats=NA,

	 permutations=permutations
         )

## Observed treeNodf for rows (sites)
if(missing(col.tree)==FALSE){# if treeNODF is for rows, tree must be present for cols.
  temp<-treeNodf(comm, col.tree, order.rows)
  resu$treeNODF.rows      <- temp$treeNODF.rows
  resu$treeNODF.rows.pairs<- temp$treeNODF.rows.pairs
  resu$NODF.rows    <- as.numeric(nestednodf(comm, order=order.rows)$statistic["N.rows"])
  resu$difNODF.rows <- resu$treeNODF.rows - resu$NODF.rows
  resu$treeNODF.rows.aleats<-numeric(permutations)
  resu$NODF.rows.aleats    <-numeric(permutations)
  resu$difNODF.rows.aleats <-numeric(permutations)
  col.tree.aleat<-col.tree} 

## treeNODF for cols (spp)
if(missing(row.tree)==FALSE){
  temp<-treeNodf(comm, col.tree, order.rows=FALSE, row.tree, order.cols)
  resu$treeNODF.cols       <- temp$treeNODF.cols
  resu$treeNODF.cols.pairs <- temp$treeNODF.cols.pairs
  resu$NODF.cols<-as.numeric(nestednodf(comm, order=order.cols)$statistic["N.columns"])
  resu$difNODF.cols<- resu$treeNODF.cols - resu$NODF.cols
  resu$treeNODF.cols.aleats <- numeric(permutations)
  resu$NODF.cols.aleats     <- numeric(permutations)
  resu$difNODF.cols.aleats  <- numeric(permutations)
  row.tree.aleat<-row.tree} 

## treeNODF for matrix (it could be substituted; mat could be obtained in the previous steps)
if(missing(col.tree)==FALSE & missing(row.tree)==FALSE){
  resu$treeNODF.mat<-treeNodf(comm, col.tree, order.rows, row.tree, order.cols)$treeNODF.mat
  comm.N<-comm
  if(order.rows==TRUE){comm.N<-comm.N[order(rowSums(comm.N),decreasing=T), ]}
  if(order.cols==TRUE){comm.N<-comm.N[ ,order(colSums(comm.N),decreasing=T)]}
  resu$NODF.mat<-as.numeric(nestednodf(comm.N, order=FALSE)$statistic["NODF"])
  resu$difNODF.mat<- resu$treeNODF.mat - resu$NODF.mat
  resu$treeNODF.mat.aleats <- numeric(permutations)
  resu$NODF.mat.aleats     <- numeric(permutations)
  resu$difNODF.mat.aleats  <- numeric(permutations)
  col.tree.aleat<-col.tree
  row.tree.aleat<-row.tree
} 

comm.aleat<-comm

for(i in 1:permutations){
## Null models
   if(null.model=="perm.rows") {comm.aleat    <-comm[sample(1:sites),] }
   if(null.model=="perm.cols") {comm.aleat    <-comm[, sample(1:spp)]}
   if(null.model=="perm.rc")   {comm.aleat    <-comm[sample(1:sites),]
                                comm.aleat    <-comm.aleat[, sample(1:spp)]}
   if(null.model=="ff")        {comm.aleat    <-commsimulator(comm, method="quasiswap")}

   if(null.model=="perm.tip.cols"){
     col.tree.aleat$tip.label<-col.tree$tip.label[sample(length(col.tree$tip.label))]} 
  
   if(null.model=="perm.tip.rows"){
     row.tree.aleat$tip.label<-row.tree$tip.label[sample(length(row.tree$tip.label))]} 

   if(null.model=="perm.tip.rc"){
     col.tree.aleat$tip.label<-col.tree$tip.label[sample(length(col.tree$tip.label))]
     row.tree.aleat$tip.label<-row.tree$tip.label[sample(length(row.tree$tip.label))]} 


## Calculations of statistics
   if(missing(col.tree)==FALSE){
      resu$treeNODF.rows.aleats[i]<-treeNodf(comm=comm.aleat, 
                                      col.tree=col.tree.aleat, order.rows=FALSE)$treeNODF.rows 
      resu$NODF.rows.aleats[i]<-as.numeric(nestednodf(comm.aleat, order=FALSE)$statistic['N.rows'])}

   if(missing(row.tree)==FALSE){
      resu$treeNODF.cols.aleats[i]<-treeNodf(comm=comm.aleat, col.tree, order.rows=FALSE,
                              row.tree=row.tree.aleat, order.cols=FALSE)$treeNODF.cols  ##although info on col.tree is not used, it should be present here in the list of arguments.
      resu$NODF.cols.aleats[i]<-as.numeric(nestednodf(comm.aleat, order=FALSE)$statistic['N.columns'])}

   if(missing(col.tree)==FALSE & missing(row.tree)==FALSE) {
      resu$treeNODF.mat.aleats[i]<-treeNodf(comm=comm.aleat,
                                         col.tree=col.tree.aleat, order.rows=FALSE,
                                         row.tree=row.tree.aleat, order.cols=FALSE)$treeNODF.mat 
      resu$NODF.mat.aleats[i]<-as.numeric(nestednodf(comm.aleat, order=FALSE)$statistic['NODF'])}

   } # close for aleats.

resu$difNODF.rows.aleats<- resu$treeNODF.rows.aleats  -  resu$NODF.rows.aleats
resu$difNODF.cols.aleats<- resu$treeNODF.cols.aleats  -  resu$NODF.cols.aleats
resu$difNODF.mat.aleats <- resu$treeNODF.mat.aleats   -  resu$NODF.mat.aleats


class(resu)<-"treeNodf.test"
resu
}


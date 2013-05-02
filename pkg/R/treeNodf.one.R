## treeNodf.one ==>> internal function ##
treeNodf.one<-function(comm, tree, order){
sites <- nrow(comm)
spp<-ncol(comm)
tree<-as.phylo(tree)
require(picante)
PDs<-pd(comm, tree)$PD

if(order==TRUE){ 
   ord<-order(PDs, decreasing=TRUE)
   comm<-comm[ord, ]
   PDs<-pd(comm, tree)$PD}
  
nodf.dist<-matrix(NA, sites, sites)        ## To store dist. results
rownames(nodf.dist) <- rownames(comm)
colnames(nodf.dist) <- rownames(comm)

for(i in 1:(sites - 1)) {
   pd.set<-PDs[i]

   for(j in (i + 1):sites) {
      pd.subset<-PDs[j]

      if(pd.subset>=pd.set){one.nodf.par<-0}
         else{
            comb<-colSums(comm[c(i,j),])
            comb<-ifelse(comb>0,1,0)
            comb<-matrix(comb, 1,)
            colnames(comb)<-colnames(comm)
            pd.comb<-pd(comb, tree)$PD
            pd.shared<- pd.set + pd.subset - pd.comb
            one.nodf.par<-pd.shared/pd.subset   
         }#close else
   
   nodf.dist[j,i]<-one.nodf.par
   }# closes for j
}# closes for i

nodf.dist<-as.dist(nodf.dist*100)
oneNODF<-mean(nodf.dist)
return(list(nodf.dist=nodf.dist, oneNODF=oneNODF))
}


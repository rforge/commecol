treeNodf<-function (comm, col.tree, order.rows=FALSE, row.tree, order.cols=FALSE) {

comm <- ifelse(comm>0,1,0)
comm <- as.matrix(comm)
resu<-list(
         treeNODF.rows=NA,
         treeNODF.cols=NA,
         treeNODF.mat=NA,
         treeNODF.rows.pairs=NA,
         treeNODF.cols.pairs=NA        
         )

# treeNodf to rows (sites).
if(missing(col.tree)==FALSE){
  if (is.null(col.tree$edge.length)) {stop("Tree has no branch lengths, cannot compute BL-diversity")}
  if (!is.rooted(col.tree))   {stop("Rooted tree required for phylosor/treeNodf calculation")}
  one.rows<-treeNodf.one(comm, tree=col.tree, order=order.rows) 
  resu$treeNODF.rows      <-one.rows$oneNODF
  resu$treeNODF.rows.pairs<-one.rows$nodf.dist
  }

# treeNodf to colums (species)
if(missing(row.tree)==FALSE){
  if (is.null(row.tree$edge.length)) {stop("Tree has no branch lengths, cannot compute BL-diversity")}
  if (!is.rooted(row.tree))   {stop("Rooted tree required for phylosor/treeNodf calculation")}
  one.cols<-treeNodf.one(t(comm), tree=row.tree, order=order.cols) 
  resu$treeNODF.cols=one.cols$oneNODF
  resu$treeNODF.cols.pairs=one.cols$nodf.dist
  }

# treeNODF to both margins
if(missing(col.tree)==FALSE & missing(row.tree)==FALSE) {
  if (is.null(col.tree$edge.length)) {stop("Tree has no branch lengths, cannot compute BL-diversity")}
  if (!is.rooted(col.tree)) {stop("Rooted tree required for phylosor/treeNodf calculation")}
  if (is.null(row.tree$edge.length)) {stop("Tree has no branch lengths, cannot compute BL-diversity")}
  if (!is.rooted(row.tree)) {stop("Rooted tree required for phylosor/treeNodf calculation")}

  tot<-sum(sum(one.rows$nodf.dist), sum(one.cols$nodf.dist))
  ndist<-sum(length(one.rows$nodf.dist), length(one.cols$nodf.dist))
  resu$treeNODF.mat<-tot/ndist 
  }

class(resu)<-"treeNodf"
resu
}






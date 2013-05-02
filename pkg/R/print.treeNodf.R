print.treeNodf<- function (x, digits=4, ...){
cat("treeNODF rows   :", format(x$treeNODF.rows, ...), "\n")
cat("treeNODF columns:", format(x$treeNODF.cols, ...), "\n")
cat("treeNODF matrix :", format(x$treeNODF.mat, ...) , "\n")

invisible(x)
}


\name{part.p.phylo}
\alias{part.p.phylo}
\title{Partition of pair-wise dissimilarity indices using phylogenetic data}
\description{This function computes the partition of pair-wise Sorensen and Jaccard dissimilarity indices described by Baselga (2011, 2012), but adapted for phylogenetic data. The two resulting components are dissimilarities due to turnover and nestednes}

\usage{part.p.phylo(comm, tree, index.family = "sorensen")}

\arguments{
  \item{comm }{Dataframe or matrix with samples in rows and species in columns.}
  \item{tree }{A phylogenetic tree containing all species listed in comm.}
  \item{index.family }{The family of dissimilarity indices to be partitionated. Partial match to "sorensen" or "jaccard".}
}

\value{A list including three dissimilarity matrices. The nomenclature of the matrices follows Baselga (2012). 
   For the jaccard:
  \item{jac }{Jaccard dissimilarity matrix.}
  \item{jtu }{The turnover component of the Jaccard index.}
  \item{jne }{The nestedness component of the Jaccard index.} 

  For the sorensen:
  \item{sor }{Sorensen dissimilarity matrix.}
  \item{sim }{The turnover component of the Sorensen index (Simpson index).}
  \item{sne }{The nestedness component of the Sorensen index.}
}

\references{
Baselga, A. 2010. Partitioning the turnover and nestdness components of beta diversity. Global Ecology and Biogeography 19, 134-143.

Baselga, A. 2012. The relationship between species replacement, dissimilarity derived from nestedness and nestedness. Global Ecology and Biogeography, in press.
}

\author{Adriano Sanches Melo, with part of the code borrowed from \code{\link[picante]{phylosor}} (package picante).}

\seealso{\code{\link{part.m.phylo}} for multi-sample partitions.}

\examples{
library(picante)
data(sites4.6)
data(tree6)
part.p.phylo(comm=sites4.6, tree=tree6, index.family="sorensen")
}

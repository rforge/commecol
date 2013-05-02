\name{treeNodf}
\alias{treeNodf}
\alias{treeNodf.test}
\title{Tree-like Nestedness of Ecological Metacommunities}

\description{This function implements a version of NODF (Nested Overlap and Decreasing Fill; Almeida-Neto et al. 2008) metric to quantify nestedness in metacommunities that takes into account relatedness among objects expressed as a tree-like object.}

\usage{
  treeNodf     (comm, col.tree, order.rows=FALSE, row.tree, order.cols=FALSE)
  treeNodf.test(comm, col.tree, order.rows=FALSE, row.tree, order.cols=FALSE,
                                      null.model="perm.rows", permutations=999)
}

\arguments{
  \item{comm}{Dataframe or matrix with samples in rows and species in columns.}
  \item{col.tree}{A tree-like object containing all species listed in \code{comm}. This tree will be used to quantify (and test) nestedness among objects in rows (e.g. sites).}
  \item{order.rows}{Should rows of \code{comm} be ordered by decreasing Branch-Length (BL) Diversity? See details below.}
  \item{row.tree}{A tree-like object containing all row objects (e.g. sites) listed in \code{comm}. This tree will be used to quantify (and test) nestedness among objects in columns (e.g. species).}
  \item{order.cols}{Should columns of \code{comm} be ordered by decreasing Branch-Length (BL) Diversity? See details below.}
  \item{null.model}{Seven null models are currently implemented, "perm.rows", "perm.cols", "perm.rc", "perm.tip.cols", "perm.tip.rows", "perm.tip.rc" and "ff". See details.}
  \item{permutations}{Number of permutations of the \code{null.model} to assess significance.}
}

\details{This is a direct extension of the NODF metric used to quantify nestedness in metacommunities using presence-absence data. NODF measures the proportion of the species richness present in a species-poor community which is present in a species-rich community. The tree-like version of this metric uses Branch-Length (BL) as a measure of diversity (instead of species richness used in NODF). For phylogenetic trees (a cladogram), BL is the Phylogenetic Diversity (PD, sensu Faith 1992). In this case, treeNODF (or phyloNODF) measures the proportion of the BL (or PD) in a BL-poor (or PD-poor) community that is present in a BL-rich (or PD-rich) community. The same reasoning applies to other tree-like objects such as functional dendrograms (Petchey & Gaston 2006).

The NODF and the treeNODF are calculated for each pair of objects in a given matrix dimension. For instance, for a dataset including 5 sites in its rows, 10 (5*4/2) pairs of values are calculated. The NODF or treeNODF for rows is simply the average of these 10 values. The same can be applied for columns. For instance, if columns represent species, one may assess whether there are nestedness in species frequencies (for NODF) or for the environmental conditions where they occur (envNODF), in the latter case using a tree-like object that depicts resemblance among sites (i.e. a dendrogram where sites are classified according to environmental variables of interest). Finally, the NODF and the treeNODF can be calculated for both rows and columns. In this case, two tree-like objects must be provided. The option to test rows (sites), columns (species) or both will depend exclusively on the hypothesis the user have raised regarding the nestedness structure of his study system.

NODF and its implementation in R \code{\link[vegan]{nestednodf}} allows one to order rows and columns by frequencies (species richness for sites and species incidences for species). However, evidence of nestedness in a matrix automatically ordered by frequencies does not allows a proper inference of the mechanism generating nestedness (see Almeida-Neto et al 2008, Ulrich et al. 2009). Accordingly, better inferences of mechanisms generating nestdness and tree-nestedness should be done by ordering communities (rows) or species (columns) accordingly to a hypothesis raised \emph{a priori} (and not by their S or BLs).

The "perm.rows" null model permute rows (sites) and is useful to test a site by species matrix in which rows (sites) were ordered by an \emph{a priori} hypothesis (e.g. island area, Lomolino 1996). The "perm.cols" null model permute columns (species) and is useful to test a site by species matrix in which columns (species) were ordered by an \emph{a priori} hypothesis (e.g. species body size). These two models must not be used if diversity (S or BL) is used to order communtiy data. This is because simulated values of the statistic will lower or equal to the observed one, but never higher.

The "perm.tip.cols" null model shuffles species (columns) labels across tips of the tree-like object. This null model should be useful to test the effect of resemblance among species (columns) to the treeNODF result. Similarly, the "perm.tip.rows" model shuffles tip labels of the row.tree (usually, sites). The "perm.tip.rc" model shuffles tip labels of both trees.

The "ff" null model is a popular choice in the literature on species co-occurrence analysis. It maintains sum of rows (species richness of sites) and sum of columns (species frequencies) fixed. Notice, however, that treeNODF makes inferences about BL and this type of "diversity" is affected not only by species richness but also by the resemblance among species (or sites) present in communities. The function \code{\link[vegan]{commsimulator}} is used to shuffle matrices according to the "ff" null model using the "quasiswap" option.

}

\value{
   For the \code{treeNodf}:
  \item{treeNODF }{The average of \code{treeNODF.pairs} when treeNODF for both rows and columns are calculated. This is the treeNODF for the entire dataset.}
  \item{treeNODF.rows }{The average of \code{treeNODF.pairs} treeNODF for rows.}
  \item{treeNODF.cols }{The average of \code{treeNODF.pairs} treeNODF for columns.} 
  \item{treeNODF.pairs.rows }{A triangular matrix of treeNODFs for each pair of rows (sites). Each value represents the treeNODF of the site showed in its row in relation to the site showed in its column. Thus, values in the first column represents the treeNODFs of each community in relation to the community supposed to have the highest BL.}
  \item{treeNODF.pairs.cols }{A triangular matrix of treeNODFs for each pair of columns (species). Each value represents the treeNODF of the species showed in its column in relation to the site showed in its row. Thus, values in the first column represents the treeNODFs of each species in relation to the species supposed to have the highest BL (that is, the species supposedly to occurr in the widest set of conditions).}



  For the \code{treeNodf.test}:
   A data frame including observed statistics (Obs), mean statitics from simulated data (M.aleat), standard deviation of simulated data (SD.aleat), Z statistic ((Obs-M.aleat)/SD.aleat), and p-values (one-tail test). Data frames will be produced for rows, columns or both depending on input data. In the latter case, data frames are included in a list.
  Probabilities are calculated as the proportion of cases (actually [cases+1]/[permutations+1]) in which statistics for rows generated by the chosen null model were higher than the observed one.
   Results for three statistics are provided: treeNODF, NODF (or simply NODF) and difNODF (difNODF = treeNODF - NODF). difNODF is intended to measure nestedness due to relatedness after taking into account nestedness due to species composition. difNODF is calculated simply as the difference between treeNODF and NODF. The same simulated matrices are used to obtain treeNODF and NODF values, the latter using function \code{\link[vegan]{nestednodf}}.
  } 


\references{
Almeida-Neto M., P. Guimaraes, P.R. Guimaraes, R.D. Loyola & W. Ulrich. 2008. A consistent metric for nestedness analysis in ecological systems: reconciling concept and measurement. Oikos 117:1227-1239.

Faith, D.P. 1992. Conservation evaluation and phylogenetic diversity. Biological Conservation 61:1-10.

Lomolino, M.V. 1996. Investigatin causality of nestedness of insular communities: selective immigration or extinctions? Journal of Biogeography 23:699-703.  

Petchey, O.L. & K.J. Gaston. 2006. Functional diversity: back to basics and looking forward. Ecology Letters 9:741-758.

Ulrich, W., M. Almeida-Neto & N.J. Gotelli. 2009. A comsumer's guide to nestedness analysis. Oikos 118:3-17.}

\author{Adriano Sanches Melo}

\seealso{\code{\link[picante]{pd}} (package picante), \code{\link[betapart]{beta.multi}} (package betapart)}

\examples{
library(picante)
data(sites5.6)
data(tree6)
     treeNodf(sites5.6, col.tree=tree6, order.rows=FALSE)
treeNodf.test(sites5.6, col.tree=tree6, order.rows=FALSE, null.model="perm.rows", permutations=99)

alt5<-vegdist(1:5, method="euclidean")
alt5<-hclust(alt5)
alt5<-as.phylo(alt5)
alt5$tip.label<-rownames(sites5.6)
     treeNodf(sites5.6, row.tree=alt5, order.cols=FALSE)
treeNodf.test(sites5.6, row.tree=alt5, order.cols=FALSE, null.model="perm.cols",permutations=99)

treeNodf(sites5.6, col.tree=tree6, order.rows=FALSE, row.tree=alt5, order.cols=FALSE)
treeNodf.test(sites5.6, col.tree=tree6, order.rows=FALSE, row.tree=alt5, order.cols=FALSE, null.model="perm.rc", permutations=99)
treeNodf.test(sites5.6, col.tree=tree6, order.rows=FALSE, row.tree=alt5, order.cols=FALSE, null.model="ff", permutations=99)
}


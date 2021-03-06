#' Diversity Stats
#'
#' This function runs stats for alpha and beta diversity
#'
#' @param newmap (Required).
#'  table of metadata.
#'
#' @param newtable (Required).
#' @export
#' @importFrom stats prcomp
#'  table of data.

# metadata table
diversity <- function(newmap,newtable){
#newmap <- read.table("Metadata_common.txt")
#newtable <- read.table("Sequence_table_common.rds")

# get counts
newmap$count <- rowSums(newtable)

# 0 out NA cells
newtable[is.na(newtable)] <- 0
newtable2 <- t(newtable)
## clr + imputation function
## Borrowed from Gabe
## Note, this assumes taxa as rows and samples as columns
clr_transform <- function(x){
  clr.taxa <- x
  clr.taxa = t(clr.taxa); eps = 0.5
  clr.taxa = clr.taxa*(1 -rowSums(clr.taxa==0)*eps/rowSums(clr.taxa))
  clr.taxa[clr.taxa==0]=eps
  clr.taxa = sweep(clr.taxa,1,rowSums(clr.taxa),'/');
  ls = log(clr.taxa)
  clr.taxa = t(ls - rowMeans(ls))
  clr.taxa = clr.taxa[,!is.nan(colSums(clr.taxa))]
  return(clr.taxa)
}

data.CLR <- clr_transform(newtable2)
data.CLR <- t(data.CLR)

#PCA
d.mes <- prcomp(data.CLR, scale = FALSE)
var_explained = (d.mes$sdev^2/sum(d.mes$sdev^2))*100
var_explained = format(round(var_explained, 2), nsmall = 2)

#eigenvalues
newmap$EV <- d.mes$sdev^2
brayWmeta <- cbind(newmap,d.mes$x[,1:4])


#alpha_diversity_stats
propdist <- sweep(newtable, 1, rowSums(newtable),'/')
brayWmeta$shannon <- vegan::diversity(propdist, index = "shannon")
brayWmeta$simpson <- vegan::diversity(propdist, index = "simpson")
brayWmeta$invsimpson <- vegan::diversity(propdist, index = "invsimpson")
write.table(brayWmeta, file="proportional_diversity_stats.txt", quote = FALSE)
return(brayWmeta)
}

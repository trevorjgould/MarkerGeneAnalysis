#' Create_Tables
#'
#' This function processes dada2 output and returns tables for plots and stats
#'
#' @export
#'
#' @importFrom utils write.table
#'
#' @examples
#' inputtable <- readRDS('data/seqtab_nochim.rds')
#' metadata <- read.table("data/metadata.txt", sep ='\t', comment='', head=TRUE, row.names=1, check.names=FALSE)
#' taxa <- readRDS("data/taxID.rds")
#' outtab <- Create_Tables(t1,t2,t3)

Create_Tables <- function(inputtable,metadata,taxa){
#find common names
common <- intersect(rownames(metadata),rownames(inputtable))
# get just the overlapping samples
newmap <- metadata[common,, drop = FALSE]
newtable <- inputtable[common,, drop = FALSE]
#save to file
saveRDS(newtable, file = "Sequence_table_common.rds")
write.table(newmap, file = "Metadata_common.txt")
#Taxa table
newtable1 <- t(newtable)
both <- cbind(newtable1,taxa)
#save to file
write.table(both, file = "combined_sequences_taxa.txt", sep = "\t", quote = FALSE)
return(list(newtable = newtable, newmap = newmap, combined_taxa = both))
}

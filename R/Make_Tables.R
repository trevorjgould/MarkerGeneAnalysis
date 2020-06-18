#' @export

#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
Make_Tables <- function(inputtable,metadata,taxa){
# input tables
#inputtable <- readRDS('seqtab_nochim.rds')
# input metadata
#metadata <- read.table('metadata.txt', sep = '\t', comment='', head=TRUE, row.names=1, check.names = FALSE)
# taxonomy
#taxa <- readRDS("taxID.rds")
# need to make a working directory and write into that not the library directory
# outtab <- Make_Tables(t1,t2,t3)
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
return(list(newtable = newtable, newmap = newmap, combined_taxa = (both)))
}

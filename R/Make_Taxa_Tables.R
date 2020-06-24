#' @export
#' @importFrom plyr ddply
#' @importFrom plyr numcolwise
#Setup and Functions
#library(plyr)
#library(dplyr)
#library(tidyr)
Make_Taxa_Tables <- function(newmap,y){
combined_taxa <- as.data.frame(y)
# taxonomy_tables
# files to use for taxa:
# metadata table
#metadata <- read.table("Metadata_common.txt")
# taxa
#newtable <- read.table("combined_sequences_taxa.txt", sep = "\t", check.names = FALSE)

#make split taxa tables
levels <- c("domain","phylum","class","order","family","genus","species")
n <- (ncol(combined_taxa) - 7)
`%>%` <- dplyr::`%>%`
Domain_table <- combined_taxa %>% dplyr::select(domain,1:n)
Phylum_table <- combined_taxa %>% dplyr::select(phylum,1:n)
Class_table <- combined_taxa %>% dplyr::select(class,1:n)
Order_table <- combined_taxa %>% dplyr::select(order,1:n)
Family_table <- combined_taxa %>% dplyr::select(family,1:n)
Genus_table <- combined_taxa %>% dplyr::select(genus,1:n)
Species_table <- combined_taxa %>% dplyr::select(species,1:n)
KT <- ddply(Domain_table, "combined_taxa$domain", numcolwise(sum))
PT <- ddply(Phylum_table, "combined_taxa$phylum", numcolwise(sum))
CT <- ddply(Class_table, "combined_taxa$class", numcolwise(sum))
OT <- ddply(Order_table, "combined_taxa$order", numcolwise(sum))
FT <- ddply(Family_table, "combined_taxa$family", numcolwise(sum))
GT <- ddply(Genus_table, "combined_taxa$genus", numcolwise(sum))
ST <- ddply(Species_table, "combined_taxa$species", numcolwise(sum))
KT = setNames(data.frame(t(KT[,-1])), KT[,1])
PT = setNames(data.frame(t(PT[,-1])), PT[,1])
CT = setNames(data.frame(t(CT[,-1])), CT[,1])
OT = setNames(data.frame(t(OT[,-1])), OT[,1])
FT = setNames(data.frame(t(FT[,-1])), FT[,1])
GT = setNames(data.frame(t(GT[,-1])), GT[,1])
ST = setNames(data.frame(t(ST[,-1])), ST[,1])
write.table(KT, file = "Kingdom_taxonomy.txt", quote = FALSE, sep = "\t")
write.table(PT, file = "Phylum_taxonomy.txt", quote = FALSE, sep = "\t")
write.table(CT, file = "Class_taxonomy.txt", quote = FALSE, sep = "\t")
write.table(OT, file = "Order_taxonomy.txt", quote = FALSE, sep = "\t")
write.table(FT, file = "Family_taxonomy.txt", quote = FALSE, sep = "\t")
write.table(GT, file = "Genus_taxonomy.txt", quote = FALSE, sep = "\t")
write.table(ST, file = "Species_taxonomy.txt", quote = FALSE)
return(list(KT=KT,PT=PT,CT=CT,OT=OT,FT=FT,GT=GT,ST=ST))
}

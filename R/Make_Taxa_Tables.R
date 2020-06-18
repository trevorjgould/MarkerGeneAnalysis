#' @export
#Setup and Functions
#library(plyr)
#library(dplyr)
#library(tidyr)
Make_Taxa_Tables <- function(newmap,newtable){
# taxonomy_tables
# files to use for taxa:
# metadata table
#metadata <- read.table("Metadata_common.txt")
# taxa
#newtable <- read.table("combined_sequences_taxa.txt", sep = "\t", check.names = FALSE)

#make split taxa tables
levels <- c("domain","phylum","class","order","family","genus","species")
n <- (ncol(newtable) - 7)
Domain_table <- newtable %>% select(domain,1:n)
Phylum_table <- newtable %>% select(phylum,1:n)
Class_table <- newtable %>% select(class,1:n)
Order_table <- newtable %>% select(order,1:n)
Family_table <- newtable %>% select(family,1:n)
Genus_table <- newtable %>% select(genus,1:n)
Species_table <- newtable %>% select(species,1:n)
KT <- ddply(Domain_table, "newtable$domain", numcolwise(sum))
PT <- ddply(Phylum_table, "newtable$phylum", numcolwise(sum))
CT <- ddply(Class_table, "newtable$class", numcolwise(sum))
OT <- ddply(Order_table, "newtable$order", numcolwise(sum))
FT <- ddply(Family_table, "newtable$family", numcolwise(sum))
GT <- ddply(Genus_table, "newtable$genus", numcolwise(sum))
ST <- ddply(Species_table, "newtable$species", numcolwise(sum))
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

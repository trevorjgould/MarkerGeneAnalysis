#' @export
#load libraries
#library(ggplot2)
#library(reshape2)
Taxonomy_Plots <- function(meta){
# read in tables
#meta <- read.table("Metadata_common.txt", sep = "\t", check.names = FALSE)
KT <- read.table(KT, file = "Kingdom_taxonomy.txt", sep = "\t", check.names = FALSE)
PT <- read.table(PT, file = "Phylum_taxonomy.txt", sep = "\t", check.names = FALSE)
CT <- read.table(CT, file = "Class_taxonomy.txt", sep = "\t", check.names = FALSE)
OT <- read.table(OT, file = "Order_taxonomy.txt", sep = "\t", check.names = FALSE)
FT <- read.table(FT, file = "Family_taxonomy.txt", sep = "\t", check.names = FALSE)
GT <- read.table(GT, file = "Genus_taxonomy.txt", sep = "\t", check.names = FALSE)

# Here we are taking the taxonomy tables created above
# taxa with less than a sum of 0.1 total proportion over all samples are merged
# into a column of OTHER with the NA column if it is present.
# NEXT IS TO DYNAMICALLY SET THE 0.1 TO LEAVE 20 TAXA TOTAL WITH OTHER AS REST.
taxa_list <- list(KT,PT,CT,OT,FT,GT)
taxa_names <- list("Kingdom","Phylum","Class","Order","Family","Genus")
for (x in 3:6){
XT_other = 0
xt=0
XTtax=0
name_label <- taxa_names[x]
xt=as.data.frame(taxa_list[x])
XTtax = sweep(xt, 1, rowSums(xt),'/')
# KEEP TOP TAXA AND SORT REST INTO OTHER CATEGORY AND PLOT
# get taxa that are present > 0.1
XT_other <- XTtax[,(colSums(XTtax)>=0.1)]
# sum taxa less then 0.1

if (table(colSums(XTtax)<0.1)["TRUE"] > 1){
XT_other$other <- rowSums(XTtax[,(colSums(XTtax)<0.1)])

#
if("NA." %in% colnames(XT_other))
{
cat("Merging NA column with Other!\n");
XT_other$Other <- XT_other$'NA.' + XT_other$other
# remove columns NA and other
XT_other <- XT_other[ , -which(names(XT_other) %in% c("NA.","other"))]
}
b <- sum(nrow(XT_other))
a <- as.integer(sum(rowSums(XT_other)))

if(a==b){
cat("taxa are looking good\n")
}
if(a!=b){
 cat("taxa do not sum to 100. there is something wrong\n")
}
}
both <- cbind(XT_other,meta)
both$Samples <- row.names(meta)
melted <- reshape2::melt(both, id.vars = c(colnames(meta)))
p <- ggplot2::ggplot(melted, aes(Samples, (value*100), fill = variable)) + geom_bar(stat='identity')+ ylab("Percent") + theme_bw() + theme(legend.position = "bottom") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_discrete(name = name_label)

#V1p <- p + facet_grid(.~V1, scales = "free_x")
#V2p <- p + facet_grid(.~V2, scales = "free_x")
#V3p <- p + facet_grid(.~V3, scales = "free_x")
#filename = paste0(name_label,"-",x,"-V1_split_taxonomy_other.png")
#filename2 = paste0(name_label,"-",x,"-V2_split_taxonomy_other.png")
#filename3 = paste0(name_label,"-",x,"-V3_split_taxonomy_other.png")
#ggsave(V1p, file = filename, dpi  = 800, width = 10, height = 8, units = "in")
#ggsave(V2p, file = filename2, dpi  = 800, width = 10, height = 8, units = "in")
#ggsave(V3p, file = filename3, dpi  = 800, width = 10, height = 8, units = "in")
filename <- paste0(name_label,"_taxonomy_other.png")
ggplot2::ggsave(p, file = filename, dpi  = 800, width = 10, height = 8, units = "in")
}
}
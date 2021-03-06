![logo](/man/images/Picture1.png)
**The output from this pipeline is:**
1) combined tables for use in analysis
2) plots
 - alpha diversity
 - beta diversity
 - taxonomy plots
3) statistical analysis
4) summary plots

**Processing of 16S sequences**
  - dada2_16S_processing.pbs > dada2_version2.R

- User Provided
  - metadata.txt


**Example:**
```
# input dada2 sequence table
t1 <- readRDS('seqtab_nochim.rds')
# input metadata
t2 <- read.table('metadata.txt', sep = '\t', comment='', head=TRUE, row.names=1, check.names = FALSE)
# input taxonomy
t3 <- readRDS("taxID.rds")

outtab <- Create_Tables(t1,t2,t3)
combined_taxa <- read.table(file = "combined_sequences_taxa.txt", sep = "\t")

taxa_out <- Make_Taxa_Tables("combined_sequences_taxa.txt")
Taxonomy_Plots(outtab$newtable)
sequence_count_table <- read.delim("sequence_process_summary.txt", row.names=1)
sequence_count_plot(sequence_count_table)
brayWmeta <- diversity(outtab$newmap,outtab$newtable)
Diversity_Plots(brayWmeta, outtab$newmap)
```

**Run Dada2**
input | output
--------- | ---------
sequence data | seqtab_nochim.rds
 . | taxID.rds


**You need the following files**
1) seqtab output file from dada2
2) metadata with sample names matching seqtab output **PROVIDED BY USER**
3) taxonomy file with sequences matching seqtab output and taxa output from IDtaxa in R

**Make_Tables.R**
input | output
--------- | ---------
seqtab_nochim.rds | Sequence_table_common.rds
metadata.txt | Metadata_common.txt
taxID.rds | combined_taxa.txt

**Make_Taxa_Tables.R**
input | output
--------- | ---------
Metadata_common.txt | Kingdom_taxonomy.txt
combined_taxa.txt | Phylum_taxonomy.txt
.  | Class_taxonomy.txt
.  | Order_taxonomy.txt
.  | Family_taxonomy.txt
.  | Genus_taxonomy.txt

**Taxonomy_Plots.R**
input | output
--------- | ---------
Kingdom_taxonomy.txt | Kingdom_taxonomy_other.png
Phylum_taxonomy.txt | Phylum_taxonomy_other.png
Class_taxonomy.txt | Class_taxonomy_other.png
Order_taxonomy.txt | Order_taxonomy_other.png
Family_taxonomy.txt | Family_taxonomy_other.png
Genus_taxonomy.txt | Genus_taxonomy_other.png
Metadata_common.txt | 

**Diversity.R**
input | output
--------- | ---------
Sequence_table_common.rds | proportional_diversity_stats.txt
Metadata_common.txt | 

**Diversity_plots.R**
input | output
--------- | ---------
proportional_diversity_stats.txt | various plots

**Sequence_count_plot.R**
input | output
--------- | ---------
sequence_process_summary.txt | sequence_count_plot.png
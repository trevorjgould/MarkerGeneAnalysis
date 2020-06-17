#' @export
# sequence count plot
sct <- function(sequence_count_table){
#sequence_count_table <- read.delim("final_sequence_count.txt", row.names=1)
#library(reshape2)
#library(ggplot2)
sct$Samples <- row.names(sct)
melted <- reshape2::melt(sct)
p <- ggplot2::ggplot(melted, aes(variable,value, fill = variable)) + theme_bw() + geom_boxplot() + geom_point() + geom_boxplot(alpha = 0.5) + geom_path(aes(group = Samples)) + xlab("Pipeline Stage") + ylab("Sequences") + theme(legend.position="none")
ggplot2::ggsave(p, file = "sequence_count_plot.png", dpi = 800, width = 6, height = 6, units = "in")
}

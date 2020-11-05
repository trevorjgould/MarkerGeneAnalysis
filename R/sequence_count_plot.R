#' sequence count plot
#'
#' this function creates a summary plot of sequence counts
#'
#' @export

# avoiding no visible binding for global variable
value <- variable <- Samples <- NULL

# sequence count plot
sequence_count_plot <- function(sct){
#sequence_count_table <- read.delim("final_sequence_count.txt", row.names=1)
#library(reshape2)
#library(ggplot2)
sct$Samples <- row.names(sct)
melted <- reshape2::melt(sct)
p <- ggplot2::ggplot(melted, ggplot2::aes(variable,value, fill = variable)) + ggplot2::theme_bw() + ggplot2::geom_boxplot() + ggplot2::geom_point() + ggplot2::geom_boxplot(alpha = 0.5) + ggplot2::geom_path(ggplot2::aes(group = Samples)) + ggplot2::xlab("Pipeline Stage") + ggplot2::ylab("Sequences") + ggplot2::theme(legend.position="none")
ggplot2::ggsave(p, file = "sequence_count_plot.png", dpi = 800, width = 6, height = 6, units = "in")
}

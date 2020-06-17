#' @export
#library(ggplot2)
#library(reshape2)
#library(gridExtra)
#library(grid)
kelly_colors <- c("plum4", "darkorange1", "firebrick", "gold2", "burlywood3", "gray51", "springgreen4", "lightpink2", "deepskyblue4", "lightsalmon2", "mediumpurple4", "orange", "maroon", "yellow3", "brown4", "yellow4", "sienna4", "chocolate", "gray19","#74c476", "lightskyblue2")
# read in files
Diversity_Plots <- function(brayWmeta,newmap){
#brayWmeta <- readRDS('proportional_diversity_stats.txt')
#newmap <- read.table("Metadata_common.txt")

# functions
grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
        do.call(arrangeGrob, lapply(plots, function(x)
            x + theme(legend.position="none"))),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight))
}

Adiv <- function(x) {
    ShanD <- ggplot2::ggplot(brayWmeta, aes(x, shannon, colour = x))+ geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(),alpha=0.3)+ theme_bw() + theme(legend.position = "bottom") + ylab("Shannon") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())
    SimD <- ggplot2::ggplot(brayWmeta, aes(x, simpson, colour = x))+ geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(),alpha=0.3)+ theme_bw() + theme(legend.position = "bottom") + ylab("Simpson") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())
    SimI <- ggplot2::ggplot(brayWmeta, aes(x, invsimpson, colour = x))+ geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(),alpha=0.3)+ theme_bw() + theme(legend.position = "bottom") + ylab("Inverse_Simpson") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    combinded_plot <- grid_arrange_shared_legend(ShanD,SimD,SimI, ncol = 1)
    plottitle <- paste0("AlphaDiversity_plots_",x,".png")
    ggplot2::ggsave(combinded_plot, file=plottitle, dpi=800, height = 12, width = 6, units = "in")
}

# ggplot functions for PCoAs
bdiv <- function(j) {
    PC1PC2 <- ggplot2::ggplot(brayWmeta, aes_string(brayWmeta$PC1,brayWmeta$PC2, colour = j)) + geom_point(size=2) + theme_bw() + theme(legend.position = "bottom")  + xlab(paste0("PC1: ",(var_explained[1]), "% variance")) + ylab(paste0("PC2: ",(var_explained[2]), "% variance"))
    PC1PC3 <- ggplot2::ggplot(brayWmeta, aes_string(brayWmeta$PC1,brayWmeta$PC3, colour = j)) + geom_point(size=2) + theme_bw() + theme(legend.position = "bottom")  + xlab(paste0("PC1: ",(var_explained[1]), "% variance")) + ylab(paste0("PC3: ",(var_explained[3]), "% variance"))
    combinded_plot <- grid_arrange_shared_legend(PC1PC2, PC1PC3, ncol = 1)
    plottitle <- paste0("PCoA_PC12_PC13_continuous",j,".png")
    ggplot2::ggsave(combinded_plot, file=plottitle, dpi=800, height = 8, width = 6, units = "in")
}

 # make plots
lapply(colnames(newmap), Adiv)
lapply(colnames(newmap), bdiv)
}
library('ggplot2')

data <- read.csv("gmm_sweeps2.csv", header=T)
data$Accuracy <- data$Accuracy * 100

p <- ggplot(data,
            aes(x=Sweeps, y=Accuracy, group=interaction(Chains, System), colour=System)) +
     geom_line(alpha=0.4) +
     ylab("Accuracy (%)") +
     guides(colour = guide_legend(override.aes = list(alpha = 1))) +
     scale_color_manual(name="",
                        values=c("cornflowerblue", "firebrick2")) +
     scale_y_continuous(expand=c(0, 0),
                        limits=c(0, 100)) +
     theme_bw() + 
     theme(panel.grid.major = element_line(colour = "black", size=0.15)) +
     theme(panel.grid.minor = element_blank()) +
     theme(panel.grid.major.x = element_blank()) +
     theme(panel.border = element_blank()) + #rect(colour = "black", fill=NA, size=1)) +
     theme(axis.line = element_line(colour = "black")) +
     theme(text = element_text(family="Times")) +
     theme(plot.title = element_text(size = rel(2))) +
     theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) +
     theme(axis.title.x = element_text(size = rel(1.5))) +
     theme(axis.text.y = element_text(size = rel(1.5), angle = 90)) +
     theme(axis.text.x = element_text(size = rel(1.5))) +
     theme(strip.text.y = element_text(size = rel(2.5))) +
     theme(panel.spacing = unit(2, "lines")) +
     theme(legend.title = element_text(size = rel(1.5))) +
     theme(legend.text = element_text(size = rel(1.3))) +
     theme(legend.background = element_rect(fill = "transparent")) +
     theme(legend.justification=c(0.02,1.0),
           legend.position=c(0.85,0.15))               # Position legend in bottom right


ggsave("plots/gmmsweeps2.pdf", p)

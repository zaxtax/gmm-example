library(ggplot2)
library(tidyr)
library(dplyr)
library(extrafont)

suppressMessages(loadfonts())
dataFile <- "gmm_sweeps.csv"
data <- read.csv(dataFile, header=T)

pd <- position_dodge(0.1)

ci_calc <- function(sd, n, conf.lev = .95) {
    sd/sqrt(n) * qt(conf.lev/2 + .5, n - 1)
}

data <- data %>%
    group_by(System,Sweeps) %>%
    summarise_each(funs(mean,
                        sd,
                        se=sd(.)/sqrt(n()),
                        ci=ci_calc(sd(.),n())),
                   Accuracy) %>%
    rename(Accuracy=mean)

theming <- theme_bw() +
        #ggtitle("Run times for Gaussian Mixture Model") +
        #expand_limits(x=0,y=0) +                        # Expand y range
        #scale_y_continuous(expand = c(0, 0), limits= c(0,4)) +
        ##scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
    
        theme(panel.grid.major = element_line(colour = "black", size=0.15)) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.grid.major.x = element_blank()) +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
        theme(text = element_text(family="Times")) +
        theme(plot.title = element_text(size = rel(2))) +
        #theme(plot.margin = margin(0.5, 1, 0.5, 1, "cm")) +
        theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) +
        theme(axis.title.x = element_text(size = rel(1.5))) +
        theme(axis.text.y = element_text(size = rel(1.5), angle = 90)) +
        theme(axis.text.x = element_text(size = rel(1.5), hjust = 1)) +
        theme(legend.title = element_text(size = rel(1.5))) +
        theme(legend.text = element_text(size = rel(1.3))) +
        theme(legend.background = element_rect(fill = "transparent")) +
    
        theme(legend.justification=c(0.02,1.0),
              legend.position=c(0.02,1.0))               # Position legend in bottom right

pAcc <- ggplot(data, aes(x=Sweeps, y=Accuracy, colour=System, group=System)) +
        geom_errorbar(aes(ymin=Accuracy-se, ymax=Accuracy+se),
                      colour="black", width=.1, position=pd) +
        geom_line(position=pd) +
        #geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
        xlab("Sweeps") +
        ylab("Accuracy (%)") +
        geom_point(aes(shape=System), size=3) +
        scale_shape(name="") +
        scale_color_hue(name="", l=40) +
        ## scale_x_continuous(expand = c(0, 0)) +
        ## scale_y_continuous(expand = c(0, 0),
        ##                    limits = c(0, 0.9)) +
        theming   

ggsave("plot.pdf", pAcc) # width=4, height=3.5)
embed_fonts("plot.pdf", outfile="plots/gmmplotsweep.pdf")

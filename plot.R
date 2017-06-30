library(ggplot2)
library(extrafont)

loadfonts()
data <- read.csv("data.csv", header=T)

pd <- position_dodge(0.1)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

data2 <- summarySE(data, measurevar="time", groupvars=c("inf_method","dataSize"))

timing.fields = c("NoBucket", "Bucket", "JAGS_init", "JAGS")
timing.labels = c("Code-generated with simplifications",
                  "… and with histogram optimization",
                  "JAGS + initialization",
                  "JAGS")

p <- ggplot(data2, aes(x=dataSize, y=time, colour=inf_method, group=inf_method)) + 
        geom_errorbar(aes(ymin=time-se, ymax=time+se), colour="black", width=.1, position=pd) +
        geom_line(position=pd) +
        #geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
        xlab("Data size") +
        ylab("Run time (secs)") +
        geom_point(aes(shape=inf_method), size=3) +
        scale_shape(name="",    # Legend label, use darker colors
                    breaks=timing.fields,
                    labels=timing.labels) +
        ## scale_color_hue(name="",    # Legend label, use darker colors
        ##                 breaks=timing.fields,
        ##                 labels=timing.labels,
        ##                 l=40) +
        scale_color_manual(name="",
                           breaks=timing.fields,
                           labels=timing.labels,
                           values=c("navyblue", "red3", "firebrick2", "blueviolet")) +
    
        #ggtitle("Run times for Gaussian Mixture Model") +
        #expand_limits(x=0,y=0) +                        # Expand y range
        scale_y_continuous(expand = c(0, 0), limits= c(0,4)) +
        ##scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
        theme_bw() +
    
        theme(panel.grid.major = element_line(colour = "black", size=0.15)) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.grid.major.x = element_blank()) +
        theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
        theme(text = element_text(family="Times")) +
        theme(plot.title = element_text(size = rel(2))) +
        theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) +
        theme(axis.title.x = element_text(size = rel(1.5))) +
        theme(axis.text.y = element_text(size = rel(1.5), angle = 90)) +
        theme(axis.text.x = element_text(size = rel(1.5))) +
        theme(legend.title = element_text(size = rel(1.5))) +
        theme(legend.text = element_text(size = rel(1.3))) +
        theme(legend.background = element_rect(fill = "transparent")) +
    
        theme(legend.justification=c(0.02,1.0),
              legend.position=c(0.02,1.0))               # Position legend in bottom right

ggsave("gmm_plot_cm.pdf", p) # width=4, height=3.5)
embed_fonts("gmm_plot_cm.pdf", outfile="gmmplot.pdf")

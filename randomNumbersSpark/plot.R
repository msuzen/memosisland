# 
#  Plot the benchmark from Spark Gaussian Random numbers
# 
#  (c) 2017
#  GPLv3
#
library(grid)
library(gridExtra)
library(ggplot2)
library(reshape)
bench_df           <- read.csv2("bench.csv", header=FALSE, sep=",", colClasses="numeric")
colnames(bench_df) <- c("N", "no", "yes")
bench_df2          <- reshape2:::melt(bench_df,
                                      measure.vars=c("no","yes"))
colnames(bench_df2) <- c("N", "repartion", "time")
bench_df2$N    <-  as.numeric(as.vector(bench_df2$N))
bench_df2$time <-  as.numeric(bench_df2$time)
gt <-  theme(
             panel.background = element_blank(), 
             axis.text.x      = element_text(face="bold", color="#000000", size=11),
             axis.text.y      = element_text(face="bold", color="#000000", size=11),
             axis.title.x     = element_text(face="bold", color="#000000", size=11),
             axis.title.y     = element_text(face="bold", color="#000000", size=11)
            )  
p1                 <- ggplot(bench_df2,
				aes(x=N, y=time, colour=repartion)) +
		             geom_smooth(formula="y~x") + xlab("Number of random draws") + ylab("Wall Clock (Seconds)") +
                             ggtitle("Effect of repartition in count: Gaussian Random Numbers") + 
                             gt
grid.newpage()
footnote <- "(c) 2017, Mehmet Suzen : http://memosisland.blogspot.de/"
g <- arrangeGrob(p1, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)))

png(file="spark_repartition_random.png")
grid.draw(g)
dev.off

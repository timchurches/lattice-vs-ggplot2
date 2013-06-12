########## R script: CIcovPlot-ggplot2.R ##########

# For plotting the results of the confidence
# interval coverage simulation study for
# the Hall, Pham, Wand & Wang paper,
# using ggplot2.

# Last changed: 11 JUN 2013

require(ggplot2)

# Read in data
dataMat <- read.table("CIcovRes.txt",header=TRUE)

# Define label expressions
distn.labels <- c('X[ij]*" ~ "*N(0,1)',
                  'X[ij]*" ~ "*Uniform(-1,1)' )
sett.labels <-  c(expression(paste(beta[0]^0 == -0.3, ", ", beta[1]^0 == 0.2, ", ", (sigma^2)^0==0.5),
                             paste(beta[0]^0 == 2.2, ", ", beta[1]^0 == -0.1, ", ", (sigma^2)^0==0.16),
                             paste(beta[0]^0 == 1.2, ", ", beta[1]^0 ==  0.4, ", ", (sigma^2)^0==0.1),
                             paste(beta[0]^0 == 0.02, ", ", beta[1]^0 == 1.3, ", ", (sigma^2)^0==1),
                             paste(beta[0]^0 == -0.3, ", ", beta[1]^0 == 0.2, ", ", (sigma^2)^0==0.1,)))
prm.labels <- c(expression(paste(beta[0]^0),
                           paste(beta[1]^0),
                           paste((sigma^2)^0)))

# Set the value labels
dataMat$distn <- factor(dataMat$distn, levels=c("norm","unif"), labels=distn.labels)
dataMat$sett <- factor(dataMat$sett, levels=LETTERS[1:5], labels=sett.labels)

# Create the plot
p <- ggplot(data=dataMat, aes(x=m, y=cov/10, linetype=prm)) 
p <- p + geom_line() 
p <- p + scale_x_continuous(breaks=seq(200,1000,200))
p <- p + scale_y_continuous(breaks=seq(85,95,5)) + coord_cartesian(ylim=c(80,100))
p <- p + scale_linetype_manual(values=1:3, labels=prm.labels, guide=guide_legend(title=NULL,keywidth=3)) 
p <- p + geom_hline(aes(yintercept=95),size=2,alpha=0.3)
p <- p + facet_grid(distn ~ sett, labeller=label_parsed)
p <- p + labs(x="value of m (n is fixed at m/10)",y="coverage percentage")
p <- p + theme_bw()
p <- p + theme(legend.position="top", legend.key=element_rect(linetype=0))

# print to PDF
pdf(file="CIcovPlot-ggplot2.pdf",width=14)
print(p)
dev.off()

# print to EPS - must use Cairo device to get alpha channel support in Postscript
divFac <- 3.25
ww <- 35/divFac ; hh <- 24/divFac
cairo_ps(file="CIcovPlot-ggplot2.eps", width=ww, height=hh)
print(p)
dev.off()

####### End of script ########

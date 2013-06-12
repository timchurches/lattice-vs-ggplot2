########## R script: CIcovPlot ##########

# For plotting the results of the confidence
# interval coverage simulation study for
# the Hall, Pham, Wand & Wang paper.

# Last changed: 16 JUN 2011

createPDF <- F ; colourVersion <- T
createEPS <- F

if (createPDF) pdf("CIcovPlot.pdf",width=14)

if (createEPS) 
{
   divFac <- 2.1
   ww <- 35/divFac ; hh <- 24/divFac
   postscript("CIcovPlot.eps",paper="special",width=ww,height=hh,
                               horizontal=FALSE,pagecentre=TRUE)
}

library(lattice)

# Read in data

dataMat <- read.table("CIcovRes.txt",header=TRUE)

settVec <-  as.character(dataMat$sett)
settFac <- settVec
settFac[settVec=="A"] <- "setting A"
settFac[settVec=="B"] <- "setting B"
settFac[settVec=="C"] <- "setting C"
settFac[settVec=="D"] <- "setting D"
settFac[settVec=="E"] <- "setting E"


distnVec <-  as.character(dataMat$distn)
distnFac <- settVec
distnFac[distnVec=="norm"] <- "norm dist"
distnFac[distnVec=="unif"] <- "unif dist"

strip.math <- function(which.given,which.panel,var.name,factor.levels,...) 
{
   if (which.given==1)
   {
      fl <- expression(
              phantom(0)(beta[0]^0 == -0.3, beta[1]^0 == 0.2,(sigma^2)^0==0.5),
              phantom(0)(beta[0]^0 == 2.2, beta[1]^0 == -0.1,(sigma^2)^0==0.16),
              phantom(0)(beta[0]^0 == 1.2, beta[1]^0 ==  0.4,(sigma^2)^0==0.1),
              phantom(0)(beta[0]^0 == 0.02, beta[1]^0 == 1.3,(sigma^2)^0==1),
              phantom(0)(beta[0]^0 == -0.3, beta[1]^0 == 0.2,(sigma^2)^0==0.1))
      strip.default(which.given,which.panel,var.name,fl,...) 
   }

   if (which.given==2)
   {
      fl <- expression(X[ij]*" ~ "*N(0,1),X[ij]*" ~ "*Uniform(-1,1))
      strip.default(which.given,which.panel,var.name,fl,...) 
   }

}

if (colourVersion)
{
   colVec <- c("dodgerblue","olivedrab","magenta")
   tmp <- trellis.par.get("superpose.line")
   tmp$col <- colVec
   trellis.par.set("superpose.line",tmp)
}

ltyVec <- c(1,2,3)
if (!colourVersion)
{

   tmp <- trellis.par.get("superpose.line")
   tmp$col <- rep("black",3)
   trellis.par.set("superpose.line",tmp)

   tmp <- trellis.par.get("superpose.line")
   tmp$lty <- ltyVec 
   trellis.par.set("superpose.line",tmp)

   tmp <- trellis.par.get("superpose.line")
   tmp$lwd <- rep(2,3)
   trellis.par.set("superpose.line",tmp)

   tmp <- trellis.par.get("box.rectangle")
   tmp$col <- "black"
   trellis.par.set("box.rectangle",tmp)

   tmp <- trellis.par.get("box.umbrella")
   tmp$col <- "black"
   trellis.par.set("box.umbrella",tmp)

   tmp <- trellis.par.get("dot.symbol")
   tmp$col <- "black"
   trellis.par.set("dot.symbol",tmp)

   tmp <- trellis.par.get("plot.symbol")
   tmp$col <- "black"
   trellis.par.set("plot.symbol",tmp)

   tmp <- trellis.par.get("strip.background")
   tmp$col <- c("grey70","grey90")
   trellis.par.set("strip.background",tmp)
}

tmp <- trellis.par.get("par.xlab.text")
tmp$cex <- 1.8
trellis.par.set("par.xlab.text",tmp)

tmp <- trellis.par.get("par.ylab.text")
tmp$cex <- 1.8
trellis.par.set("par.ylab.text",tmp)

tmp <- trellis.par.get("add.text")
tmp$cex <- 1.0
trellis.par.set("add.text",tmp)

tmp <- trellis.par.get("layout.heights")
tmp$strip <- 1.3
trellis.par.set("layout.heights",tmp)

tmp <- trellis.par.get("layout.heights")
tmp$key.top <- 0.9
trellis.par.set("layout.heights",tmp)

tmp <- trellis.par.get("layout.heights")
tmp$main.key.padding <- 0.01
trellis.par.set("layout.heights",tmp)

if (createEPS)
{
   tmp <- trellis.par.get("add.text")
   tmp$cex <- 1.291
   trellis.par.set("add.text",tmp)

   tmp <- trellis.par.get("layout.heights")
   tmp$strip <- 1.9
   trellis.par.set("layout.heights",tmp)

   tmp <- trellis.par.get("par.xlab.text")
   tmp$cex <- 2.8
   trellis.par.set("par.xlab.text",tmp)

   tmp <- trellis.par.get("par.ylab.text")
   tmp$cex <- 2.8
   trellis.par.set("par.ylab.text",tmp)

   tmp <- trellis.par.get("axis.text")
   tmp$cex <- 1.5
   trellis.par.set("axis.text",tmp)
}

numTicks <- 8

xTmp <- c(100,500,1000)
yTmp <- c(83,91,98)

if (colourVersion)
{
   lwdVec <- rep(1,3)
   ltyVec <- rep(1,3)
   horizLineCol <- "navy"
}

if (!colourVersion)
{
   lwdVec <- rep(2,3)
   colVec <- rep("black",3)
   horizLineCol <- "grey55"
}


pobj <- xyplot((100*cov/1000)~m|settFac*distnFac,groups=prm,
                 data=dataMat,ylim=c(80,100),
                    strip=strip.math,
                    as.table=TRUE,
                    xlab="value of m (n is fixed at m/10)",
                    ylab="coverage percentage",
                    key = list(title="",
                      columns = 3,cex=3,
                      lines = list(lwd=lwdVec,lty=ltyVec,col=colVec),
                      text = list(c(expression(beta[0]^0),
                                    expression(beta[1]^0),
                                    expression((sigma^2)^0)))),
                    panel=function(x,y,subscripts,groups,...) 
                    {
                       vVals <- seq(100,1000,by=100) ; hVals <- seq(80,100,by=5) 
                       panel.abline(h=hVals,col="grey85")
                       panel.abline(v=vVals,col="grey85")
                       panel.abline(h=95,col=horizLineCol,lty=1,lwd=4)
                       panel.superpose(x,y,subscripts,groups,type="l")
                    }
              )

print(pobj)

############ End of CIcovPlot ############

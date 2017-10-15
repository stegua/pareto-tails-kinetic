#--------------------------------------------------------------------------------------------------------
#
# Copyright (c) 2017, by Stefano Gualandi and Giuseppe Toscani, UniPv,
#               via Ferrata, 1, Pavia, Italy, 27100
#
# - Additional material for the paper "Pareto tails in socio-economic phenomena: A kinetic description"
# - R script for producing the figure "PopulationPvalues.png"
#
#--------------------------------------------------------------------------------------------------------


# REQUIRED PACKAGE:
# https://cran.r-project.org/web/packages/poweRlaw/index.html

# FOR A TUTORIAL SEE SECTION 2 in :
# https://cran.r-project.org/web/packages/poweRlaw/vignettes/b_powerlaw_examples.pdf

# Load library
library("poweRlaw", lib.loc="~/R/win-library/3.2")


# Make plot for each country
myplot <- function(country, foo) {
  mydata = read.table(paste("D:\\Ricerca\\Pareto\\", country, "_flat.txt", sep=""))

  x <- Filter(function(x) x > 10^5, mydata$V1) 
  x <- sapply(x, function(x) 1+round(x/1000))
  
  # estimate Pareto (power law) distribution
  m_sp = displ$new(x)
  est_sp = estimate_xmin(m_sp)
  
  # set paramaters to distribution object
  m_sp$setXmin(est_sp)
  
  # prepare options for plot
  par(mar=c(3, 3, 2, 1), mgp=c(2, 0.4, 0), tck=-.01, cex.axis=0.9, las=1)
  
  # just plot input data
  plot(m_sp, pch=21, bg=4, xlim=c(100, 20000), ylim=c(0.001, 1.00),
       panel.first=grid(col="grey80"),xlab="Population size (thousands)", ylab="", 
       main = paste(foo[country],", #cities=",length(x), ", p=", round(est_sp$pars,3), sep=""))
  
  # interpolated line
  lines(m_sp, col=2, lwd=3)
}

# Prepare input data
foo <- vector(mode="list", length=6)
names(foo) <- c("usa", "japan", "italia", "polonia", "india", "canada")
foo[[1]] = "USA"
foo[[2]] = "Japan"
foo[[3]] = "Italy"
foo[[4]] = "Poland"
foo[[5]] = "India"
foo[[6]] = "Canada"

# Plot for six countries
par(mfrow = c(3, 2))  # 3 rows and 2 columns
for (i in c("usa", "japan", "italia", "polonia", "india", "canada")) {
  myplot(i, foo)
}

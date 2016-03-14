require(ggplot2)
load("Out_lower.Rdata") # site-specific model results
sl <- read.csv("Data_LowerRegionSpecies.csv", header=T) # site-specific species list
d <- out$summary

# plot temporal-change in abundance
d1 <- as.data.frame(d[1:55,]) # parameter r for 55 species
d1$Species <- sl$Name # joining species names
d2 <- subset(d1, select=c("2.5%","50%","97.5%","Species")) # selecting columns for plotting
colnames(d2) <- c("Lower95CI","Median","Upper95CI","Species") # rename columns
d3 <- d2[order(d2$Median),] # reorder by species by increasing median growth rate
d3$idx <- seq(1:nrow(d3)) # index for plotting species in rows

theme_set(theme_bw(base_size=30))
p <-  ggplot(data=d3) +
      ggtitle("Potomac River: lower region") +
      expand_limits(x=c(-0.15, 0.05)) +
      geom_errorbarh(aes(y=idx, x=0, xmin=Lower95CI, xmax=Upper95CI), height=0) +
      geom_point(aes(x=Median, y=idx), size=3)

p + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
          plot.title=element_text(size=22), axis.title.x=element_text(size=20), axis.text.x=element_text(size=18)) +
    xlab("Annual change in abundance") +
    geom_text(aes(x=-0.4, y=idx, label=Species), hjust=0, size=3) +
    geom_vline(aes(xintercept=0), lty=2, lwd=1)

# plot abundance-flow relation
d1 <- as.data.frame(d[56:110,]) # parameter b for 55 species
d1$Species <- sl$Name # joining species names
d2 <- subset(d1, select=c("2.5%","50%","97.5%","Species")) # selecting columns for plotting
colnames(d2) <- c("Lower95CI","Median","Upper95CI","Species") # rename columns
d3 <- d2[order(d3$Median),] # reorder by species by increasing median growth rate
d3$idx <- seq(1:nrow(d3)) # index for plotting species in rows

theme_set(theme_bw(base_size=30))
p <-  ggplot(data=d4) +
  ggtitle("Potomac River: lower region") +
  expand_limits(x=c(-0.15, 0.05)) +
  geom_errorbarh(aes(y=idx, x=0, xmin=Lower95CI, xmax=Upper95CI), height=0) +
  geom_point(aes(x=Median, y=idx), size=3)

p + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank(), 
          plot.title=element_text(size=22), axis.title.x=element_text(size=20), axis.text.x=element_text(size=18)) +
  xlab("Abundance reln to mean annual flow") +
  geom_text(aes(x=-0.5, y=idx, label=Species), hjust=0, size=3) +
  geom_vline(aes(xintercept=0), lty=2, lwd=1)
library(ggplot2)
library(ukhlsclean)
library(data.table)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)) )
}


#######################
#### read in the data

### plot dimensions

units <- "in"
width <- 10/1.5
height <- 5/1.5
dpi <- 600


# Factorise wave numbers
fulldata[ , wave_no := as.factor(wave_no)]

### colour palette
#colourCount = 12
#getPalette = colorRampPalette(brewer.pal(11, "PuOr"))#(colourCount)







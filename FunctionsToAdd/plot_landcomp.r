#make plot of landscape composition
library(ggplot2)
landnames <- unique(landcover$Landscape)
landscapestouse <- landcover[landcover$Landscape %in% landnames,]
landscapestouse <- landscapestouse[order(landscapestouse$Class_Name),]

classes <- landscapestouse[!duplicated(landscapestouse$Class_Name), c("Class_Name", "Red", "Green", "Blue")]
classes$hex <- rgb(classes[-1], max=1)

landplot <- ggplot() + geom_bar(aes(y = Pct_Land, x = Landscape, fill = Class_Name), data = landscapestouse,
                          stat="identity") + theme_classic() + theme(axis.title.x= element_blank(), axis.text.x = element_text(angle=90, vjust=0.5, size=12), axis.title.y= element_text(size=14)) +
                          ylab("Percent of Landscape") + scale_fill_manual(values=classes$hex)
landplot
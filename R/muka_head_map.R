########################################################
# TITLE: To plot the Muka Head site map
# AUTHOR: Yusri Yusup, PhD
# DATE: 2016-05-04
# VERSION: 1.0
# NOTE: This script downloads and plots the map using
# ggmap and google map
########################################################

####### * Installing and loading required packages ###################################################

#install.packages("ggmap")
#install.packages("gridExtra")
#install.packages("maptools")
#install.packages("rgdal")
#install.packages("raster")

#loading libraries
library(ggmap)
library(mapproj)
library(ggplot2)
library(gridExtra)
library(maptools)
library(rgdal)
library(raster)
library(grid)

#load map tools
source('R/tools/tool_map_createscale.R') # Script to source functions of scales

##### * Zoomed in map of station #####
sitemap <- get_googlemap(center = c(lon = 100.2025,lat = 5.4750), sensor=TRUE,
                         size = c(640,640), scale = 2, zoom = 15, maptype = "satellite")

map_plot <- ggmap(sitemap) +
  geom_point(aes_string(x = "100.2003",y = "5.4685"),size = 5,shape=16,colour="black")+
  geom_text(aes_string(x="100.2002",y="5.4685"),label="Muka Head Station",colour="white",size=7,
            fontface="bold",hjust=0,vjust=-1.00) +
  xlab("") + ylab("") +
  theme(plot.title = element_text(lineheight=1, face="bold",size = 25, colour = "grey20"),
        axis.line=element_blank(),
        panel.border = element_rect(colour="grey20",fill=NA,size=0.5),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) +
  scaleBar(lon = 100.19, lat = 5.486, distanceLon = 1, distanceLat = 0.05, distanceLegend = 0.15,
           dist.unit = "km", orientation = FALSE)
#Plot site map
png(filename = "figs/muka_head_plot_zoom.png",height=8,width=8,
    bg = "white",units='in', res = 360, family = "")
map_plot
dev.off()
rm(sitemap,map_plot)


##### * Zoomed out map of station #####
map_main <- ggmap(get_googlemap(center=c(100.2025, 5.4750),zoom=10,maptype='terrain',size = c(640,640),scale = 2,color='color',
                                extent='panel',darken=0))

map_main1 <- map_main + 
  geom_point(aes_string(x = "100.2025",y = "5.4750"), shape = 16, colour = "black", fill="black", size = 6) +  
  #geom_text(aes_string(x = "100.2025",y = "5.4750"), colour="black",size=8.5,fontface="bold",hjust=1.1,vjust=0.25)+
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = c(0.03, 0.06), # put the legend INSIDE the plot area
        legend.justification = c(0, 0),
        legend.background = element_rect(colour = F, fill = "white"),
        legend.key = element_rect (fill = F, colour = F),
        axis.title=element_text(size=14,face="bold",colour="grey19"),
        axis.text.x=element_text(size=14,face="bold",colour="grey19"),
        axis.text.y=element_text(size=14,face="bold",colour="grey19"),
        panel.border = element_rect(colour = "grey19",fill=F,size=1.2)) +
  scaleBar(lon = 99.8, lat = 5.12, distanceLon = 25, distanceLat = 2, distanceLegend = 5,
           dist.unit = "km", orientation = FALSE)
rm(map_main)


#### * Plot inset map ##################

# download MYS level 0 map from ucdavis site
mys0 <- getData("GADM", country="MYS", level=0) 

# load shapefile
oc <- readOGR(dsn="data/ne_10m_admin_0_countries", layer="ne_10m_admin_0_countries")

# to draw a rectangle above the big map
pol <- data.frame(xmin=99.8,xmax=100.6 ,ymin=5.0 ,ymax=5.85)

malaysia <- ggplot() +
  geom_polygon(data=oc,aes(long,lat,group=group),fill="grey60") +
  geom_polygon(data=mys0, aes(long,lat,group=group),colour="grey10",fill="grey90",size=0.2) +
  theme_bw() +
  labs(x=NULL,y=NULL) +
  annotate("text", x = 102.4, y = 3.75, label = "PENINSULAR\nMALAYSIA",size=5,fontface="bold") +
  annotate("text", x = 99.8, y = 2, label = "SUMATERA,\nINDONESIA",size=5,fontface="bold") +
  ggtitle("\nLOCALITY MAP") + 
  coord_equal(xlim=c(96, 107), ylim=c(0.5, 7)) +
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.6, colour = "red", size = 1, linetype = 1) +
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank(),panel.border = element_rect(colour="white",fill=FALSE),
        plot.title = element_text(lineheight=0.2, face="bold",size =18, colour = "grey20"),
        plot.background = element_rect(colour="black",fill="white",size=1),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.margin=unit(c(0.0,1,0.1,0.1),"mm")) 


fullMap <- map_main1 + 
  inset(grob = ggplotGrob(malaysia + 
                            theme(legend.position = c(0.03, 0.06), # put the legend INSIDE the plot area
                                  legend.justification = c(0, 0),
                                  legend.background = element_rect(colour = F, fill = "white"),
                                  legend.key = element_rect (fill = F, colour = F),
                                  panel.grid.major = element_blank (), # remove major grid
                                  panel.grid.minor = element_blank (),  # remove minor grid
                                  axis.text = element_blank (), 
                                  axis.title.x=element_blank(),
                                  axis.title.y=element_blank(),
                                  axis.ticks = element_blank (),
                                  panel.border = element_rect(colour = "grey19",fill=F,size=1.2),
                                  panel.background = element_rect(fill = "white",colour = "black"))), 
        xmin = 99.8,  xmax = 100.3, ymin = 5.40, ymax = 6.05) +
  theme(plot.title = element_text(face = "bold",size = 14,colour="grey19"))

png(filename = "figs/fullmap.png",height=8,width=8,
    bg = "white",units='in', res = 360, family = "")
fullMap
dev.off()
rm(malaysia,fullMap,map_main1,mys0,pol,oc)





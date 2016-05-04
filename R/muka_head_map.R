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

#load map tools
source('R/tools/tool_map_createscale.R') # Script to source functions of scales

##### * Generate plots ##################################################################################
##### * Zoomed in map of station #####
sitemap <- get_googlemap(center = c(lon = 100.2025,lat = 5.4750), sensor=TRUE,
                         size = c(640,640), scale = 2, zoom = 15, maptype = "satellite")
ggmap(sitemap)

map_plot <- ggmap(sitemap) +
  geom_point(aes_string(x = "100.2003",y = "5.4685"),size = 5,shape=16,colour="black")+
  geom_text(aes_string(x="100.2002",y="5.4685"),label="Muka Head Station",colour="white",size=7,
            fontface="bold",hjust=0,vjust=-1.00) +
            #family='Times New Roman') + 
  xlab("") + ylab("") +
  #ggtitle(paste("Keratong - Eddy Covariance Tower")) +
  theme(plot.title = element_text(lineheight=1, face="bold",size = 25, colour = "grey20"),
        axis.line=element_blank(),
        panel.border = element_rect(colour="grey20",fill=NA,size=0.5),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())

#Plot site map
png(filename = "figs/muka_head_plot_zoom.jpeg",height=12,width=12,
    bg = "white",units='in', res = 360, family = "",  type = "cairo-png")
map_plot
dev.off()





#Main map
map <- ggmap(get_googlemap(center=c(102.9327469, 2.788916588),zoom=9,maptype='terrain',size = c(640,640),scale = 2,color='color',
                           extent='panel',darken=0))

map1= map + 
  geom_point (aes_string(x = "102.9327469",y = "2.788916588"), shape = 21, colour = "black", fill="black", size = 4) +  
  geom_text(aes_string(x = "102.9327469",y = "2.788916588"),label = "Site Location",colour="black",size=8.5,fontface="bold",hjust=1.1,vjust=0.25)+
  labs(x = "Longitude", y = "Latitude") +
  theme ( legend.position = c(0.03, 0.06), # put the legend INSIDE the plot area
          legend.justification = c(0, 0),
          legend.background = element_rect(colour = F, fill = "white"),
          legend.key = element_rect (fill = F, colour = F),
          axis.title=element_text(size=14,face="bold",colour="grey19"),
          axis.text.x=element_text(size=14,face="bold",colour="grey19"),
          axis.text.y=element_text(size=14,face="bold",colour="grey19"),
          panel.border = element_rect(colour = "grey19",fill=F,size=1.2)) +
  scaleBar(lon = 100.5, lat = 1.0, distanceLon = 50, distanceLat = 5, distanceLegend = 16, dist.unit = "km", orientation = FALSE)

png(filename = "main2.png",height=12,width=12,
    bg = "white",units='in', res = 360, family = "",  type = "cairo-png")
map1
dev.off()

#Inset

mys0 <-getData("GADM", country="MYS", level=0) # download MYS level 0 map for ucdavis site

oc <- readOGR(dsn="C:/Users/user/Documents/CO2_eddy/Data//countries_shp", layer="countries")

pol<-data.frame(xmin=101.7,xmax=104.0 ,ymin=1.9 ,ymax=3.6)

p2<- ggplot()+geom_polygon(data=oc,aes(long,lat,group=group),fill="grey60")+
  geom_polygon(data=mys0, aes(long,lat,group=group),colour="grey10",fill="grey90",size=0.2)+
  theme_bw()+labs(x=NULL,y=NULL)+
  annotate("text", x = 102.4, y = 3.75, label = "PENINSULAR\nMALAYSIA",size=5,fontface="bold")+
  annotate("text", x = 99.8, y = 2, label = "SUMATERA,\nINDONESIA",size=5,fontface="bold")+
  ggtitle("LOCALITY MAP\n")+ coord_equal(xlim=c(96, 107), ylim=c(0.5, 7))+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1)+
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank(),panel.border = element_rect(colour="white",fill=FALSE),
        plot.title = element_text(lineheight=0.2, face="bold",size =18, colour = "grey20"),
        plot.background = element_rect(colour="black",fill="white",size=1),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        plot.margin=unit(c(0.0,1,0.1,0.1),"mm")) 

fullMap <- map1 + inset(grob = ggplotGrob(p2+theme( 
  legend.position = c(0.03, 0.06), # put the legend INSIDE the plot area
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
  xmin = 101.9,  xmax = 103.1, ymin = 2.0, ymax = 2.5) +
  theme(plot.title = element_text(face = "bold",size = 14,colour="grey19"))

fullMapx <- fullMap + scaleBar(lon = 102.95, lat = 2.01, distanceLon = 25, distanceLat = 2, distanceLegend = 5, dist.unit = "km", orientation = FALSE)

#print(fullMap)

png(filename = "fullmap3.png",height=12,width=12,
    bg = "white",units='in', res = 360, family = "",  type = "cairo-png")
fullMap
dev.off()

png(filename = "C:/Users/user/Documents/CO2_eddy/Fig/fullmapx3.png",height=12,width=12,
    bg = "white",units='in', res = 360, family = "",  type = "cairo-png")
fullMapx
dev.off()

#ggsave ("C:/Users/user/Documents/CO2_eddy/mapss.png", dpi = 400,w=10,h=10)






#Sitemap Ver2 (optional, more zoomed in)
sitemap2 <- get_googlemap(center = c(lon = 102.9327469,lat =2.788916588), sensor=TRUE,
                          size = c(640,640), scale = 2,zoom = 16, maptype = "terrain")

plot2 <- ggmap(sitemap2) +
  geom_point(aes_string(x = "102.9327469",y = "2.788916588"),size = 5,shape=16,colour="black")+
  geom_text(aes_string(x="102.9327469",y="2.788916588"),label="Eddy Covariance Tower",colour="black",size=10,fontface="bold",hjust=1.00,vjust=-1.00)+ xlab("") + ylab("") +
  ggtitle(paste("Keratong - Eddy Covariance Tower")) +
  theme(plot.title = element_text(lineheight=1, face="bold",size = 12, colour = "grey20"),
        axis.line=element_blank(),
        panel.border = element_rect(colour="grey20",fill=NA,size=0.5),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank())
#Plot site map Ver2
png(filename = "Keratong Site2.png",height=12,width=12,
    bg = "white",units='in', res = 360, family = "",  type = "cairo-png")
plot2
dev.off()
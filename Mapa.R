library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
Sur_America     <- st_read ("SHP/SurAmerica.shp")  
SurAmerica_utm  <- st_transform(Sur_America ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

my_bbox <- c(xmin = min(-82), xmax = max(-63),  ymin = min(-55),  ymax = max(-10))
my_bbox.m <- matrix(c(my_bbox['xmin'], my_bbox['xmin'], my_bbox['xmax'], my_bbox['xmax'], my_bbox['xmin'],  my_bbox['ymax'], my_bbox['ymin'], my_bbox['ymin'], my_bbox['ymax'], my_bbox['ymax']), ncol = 2)

my_bbox.sf <- st_geometry(st_polygon(x = list(my_bbox.m)))
st_crs(my_bbox.sf) <- 4326

Zona<- st_transform(my_bbox.sf ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
write_sf(Zona, "SHP/Zona.shp")
Zon= st_read("SHP/Zona.shp")  %>% st_as_sf()
Zona <- st_transform(Zon ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

library(rgbif)
Bertholletia<- occ_search(scientificName="Bertholletia excelsa")
Castaña            <- subset(Bertholletia$data , scientificName == "Bertholletia excelsa Bonpl.")
Castaña$image <- "PNG/Castaña.png"

library(grid)
library(png)
library(ggimage)
Castaña_img <- readPNG("PNG/Castaña.png", FALSE)
Castaña_png <- rasterGrob(Castaña_img, x = unit(0.2, "npc"),y = unit(0.1, "npc"), width = unit(0.3, "npc"))



Ximenia  <- occ_search(scientificName="Ximenia americana")
Ximenia_S            <- subset(Ximenia$data , scientificName == "Ximenia americana L.")
Ximenia_S$image <- "PNG/Ximeni.png"

Ximeni_img <- readPNG("PNG/Ximeni.png", FALSE)
Ximeni_png <- rasterGrob(Ximeni_img , x = unit(0.2, "npc"),y = unit(0.1, "npc"), width = unit(0.4, "npc"))


Polylepis  <- occ_search(scientificName="Polylepis tarapacana")
Polylepis_S            <- subset(Polylepis$data , scientificName == "Polylepis tarapacana Phil.")
Polylepis_S$image <- "PNG/Polylepis.png"

Polylepis_img <- readPNG("PNG/Polylepis.png", FALSE)
Polylepis_png <- rasterGrob(Polylepis_img , x = unit(0.2, "npc"),y = unit(0.1, "npc"), width = unit(0.4, "npc"))


Macro= ggplot()+
  geom_sf(data = SurAmerica_utm , fill=NA, color="gray30", size=0.8)+
  geom_sf(data = Zona, fill="black", color="gray", size=0.01, alpha=0.2)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 12, hjust = 0, vjust = 1, 
           label = "Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
             annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
                      label = "Pacific ocean",size = 3, family="serif", color = 
                        "black",  fontface="italic", angle=90)+
                        annotate(geom = "text", x = -60, y = -50, hjust = 0, vjust = 1, 
                                 label = "Atlantic ocean",size = 3, family="serif", color = 
                                   "black",  fontface="italic")
                                            

library(elevatr)
library(ggnewscale)
elev = get_elev_raster(SurAmerica_utm , z=6)
plot(elev)
Poligo_alt    <- crop(elev, SurAmerica_utm)                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, SurAmerica_utm)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6500)


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

summary(Geo_data_frame$alt)

Mapa1 = ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores ,
                       breaks = cortes ,
                       na.value = 'white',
                       name='Elevacion \n(msnm)')+
  geom_sf(data = SurAmerica_utm , fill=NA, color="gray30", size=0.8)+
  coord_sf(xlim = c(-82, -63), ylim = c(-55 ,-10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        
        
        legend.position = c(0.2,0.3),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        
        
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+ annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -75, y = -12, hjust = 0, vjust = 1,  label = "Peru",size = 3, family="serif", color =  "black",  fontface="italic", face = "bold")+ 
  annotate(geom = "text", x = -68, y = -19, hjust = 0, vjust = 1,  label = "Bolivia",size = 3, family="serif", color =  "black",  fontface="italic", face = "bold")+ 
  annotate(geom = "text", x = -70, y = -32, hjust = 0, vjust = 1,  label = "Argentina",size = 3, family="serif", color = "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -73, y = -40, hjust = 0, vjust = 1,  label = "Chile",size = 3, family="serif", color = "black",  fontface="italic", face = "bold", angle=90)+
  annotate(geom = "text", x = -80, y = -37, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
             annotation_custom(Castaña_png)+
  geom_point(data = Castaña, aes( x=decimalLongitude, y = decimalLatitude, color=scientificName) ,size=2, alpha=0.3, show.legend = F, color="black")+
  annotate(geom = "text", x = -80, y = -25, hjust = 0, vjust = 1, 
           label = "Bertholletia excelsa Bonpl.",size = 5, family="serif", color = 
             "black",  fontface="italic", angle=90)+
             guides(fill = guide_legend(
               title = "Elevación \nmsnm",
               
               nrow = 9,
               keywidth = 0.5,
               keyheight = 0.5,
               
               title.position = "top",
               override.aes = list(alpha = 1)
             ))+
  guides(fill = guide_legend(nrow = 4, ncol=2))


Mapa2 = ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores ,
                       breaks = cortes ,
                       na.value = 'white',
                       name='Elevación \n(msnm)')+
  geom_sf(data = SurAmerica_utm , fill=NA, color="gray30", size=0.8)+
  coord_sf(xlim = c(-82, -63), ylim = c(-55 ,-10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        
        legend.position = c(0.2,0.3),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -75, y = -12, hjust = 0, vjust = 1,  label = "Peru",size = 3, family="serif", color =  "black",  fontface="italic", face = "bold")+ 
  annotate(geom = "text", x = -68, y = -19, hjust = 0, vjust = 1,  label = "Bolivia",size = 3, family="serif", color =  "black",  fontface="italic", face = "bold")+ 
  annotate(geom = "text", x = -70, y = -32, hjust = 0, vjust = 1,  label = "Argentina",size = 3, family="serif", color = "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -73, y = -40, hjust = 0, vjust = 1,  label = "Chile",size = 3, family="serif", color = "black",  fontface="italic", face = "bold", angle=90)+
  annotate(geom = "text", x = -80, y = -37, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
             geom_point(data = Ximenia_S, aes( x=decimalLongitude, y = decimalLatitude), size = 0.04)+
  annotation_custom(Ximeni_png)+
  geom_point(data = Ximenia_S,aes( x=decimalLongitude, y = decimalLatitude, color=scientificName) ,size=2, alpha=0.3, show.legend = F, color="black")+
  annotate(geom = "text", x = -80, y = -25, hjust = 0, vjust = 1, 
           label = "Ximenia americana L.",size = 5, family="serif", color = 
             "black",  fontface="italic", angle=90)+
             guides(fill = guide_legend(
               title = "Elevacion \nmsnm",
               
               nrow = 9,
               keywidth = 0.5,
               keyheight = 0.5,
               
               title.position = "top",
               override.aes = list(alpha = 1)
             ))+
  guides(fill = guide_legend(nrow = 4, ncol=2))


Mapa3 = ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores ,
                       breaks = cortes ,
                       na.value = 'white',
                       name='Elevación \n(msnm)')+
  geom_sf(data = SurAmerica_utm , fill=NA, color="gray30", size=0.8)+
  coord_sf(xlim = c(-82, -63), ylim = c(-55 ,-10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        
        legend.position = c(0.2,0.3),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5), 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+ annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -75, y = -12, hjust = 0, vjust = 1,  label = "Peru",size = 3, family="serif", color =  "black",  fontface="italic", face = "bold")+ 
  annotate(geom = "text", x = -68, y = -19, hjust = 0, vjust = 1,  label = "Bolivia",size = 3, family="serif", color =  "black",  fontface="italic", face = "bold")+ 
  annotate(geom = "text", x = -70, y = -32, hjust = 0, vjust = 1,  label = "Argentina",size = 3, family="serif", color = "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -73, y = -40, hjust = 0, vjust = 1,  label = "Chile",size = 3, family="serif", color = "black",  fontface="italic", face = "bold", angle=90)+
  annotate(geom = "text", x = -80, y = -37, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
             annotation_custom(Polylepis_png)+
  geom_point(data = Polylepis_S,aes( x=decimalLongitude, y = decimalLatitude, color=scientificName) ,size=2, alpha=0.3, show.legend = F, color="black")+
  annotate(geom = "text", x = -80, y = -25, hjust = 0, vjust = 1, 
           label = "Polylepis tarapacana Phil.",size = 5, family="serif", color = 
             "black",  fontface="italic", angle=90)+
             guides(fill = guide_legend(
               title = "Elevacion \nmsnm",
               
               nrow = 9,
               keywidth = 0.5,
               keyheight = 0.5,
               
               title.position = "top",
               override.aes = list(alpha = 1)
             ))+
  guides(fill = guide_legend(nrow = 4, ncol=2))

library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 27), ylim = c(0, 26), expand = FALSE) +
  draw_plot(Mapa1 , width = 9, height = 26,x = 0, y = 0)+
  draw_plot(Mapa2 , width = 9, height = 26,x = 9, y = 0)+
  draw_plot(Mapa3 , width = 9, height = 26,x = 18, y = 0)+
  
  draw_plot(Macro , width = 4, height = 6,x = 0, y = 11)+
  draw_plot(Macro , width = 4, height = 6,x = 9, y = 11)+
  draw_plot(Macro , width = 4, height = 6,x = 18, y = 11)


ggsave(plot=Expo ,"Mapa de dispersion.png",units = "cm",width = 26, #alto
       height = 26, #ancho
       dpi=1200)






































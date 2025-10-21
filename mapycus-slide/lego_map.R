library(tidyverse) 
library(sf)
library(mapycusmaximus)
map<-sf::read_sf('https://github.com/BjnNowak/lego_map/raw/main/data/france_sport.gpkg')

# Create classes
clean<-map%>%
  mutate(clss=case_when(
    value<18~"1",
    value<20~"2",
    value<22~"3",
    value<24~"4",
    value<26~"5",
    TRUE~"6"
  ))

# Set color palette
pal <- c("#bb3e03","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#005f73")
# Set color background
bck <- "#001219"

# Set theme 
theme_custom <- theme_void()+
  theme(
    plot.margin = margin(1,1,10,1,"pt"),
    plot.background = element_rect(fill=bck,color=NA),
    legend.position = "bottom",
    legend.title = element_text(hjust=0.5,color="white",face="bold"),
    legend.text = element_text(color="white")
  )

# Make choropleth
ggplot(clean, aes(fill=clss))+
  geom_sf()+
  labs(fill="Member of a sport association")+
  guides(
    fill=guide_legend(
      nrow=1,
      title.position="top",
      label.position="bottom"
    )
  )+
  scale_fill_manual(
    values=pal,
    label=c("< 18 %","< 20 %","< 22 %","< 24 %","< 26 %", "≥ 26 %")
  )+
  theme_custom


# Make grid
grd<-st_make_grid(
    clean, # map name 
    n = c(60,60) # number of cells per longitude/latitude
  )%>%
  # convert back to sf object
  st_sf()%>%
  # add a unique id to each cell 
  # (will be useful later to get back centroids data)
  mutate(id=row_number())
  
# Extract centroids
cent<-grd%>%
  st_centroid()

# Intersect centroids with basemap
cent_clean<-cent%>%
  st_intersection(clean)

# Make a centroid without geom
# (convert from sf object to tibble)
cent_no_geom <- cent_clean%>%
  st_drop_geometry()

# Join with grid thanks to id column
grd_clean<-grd%>%
  #filter(id%in%sel)%>%
  left_join(cent_no_geom)


ggplot()+
  geom_sf(
    # drop_na() is one way to suppress the cells outside the country
    grd_clean%>%drop_na(), 
    mapping=aes(geometry=geometry,fill=clss)
  )+
  geom_sf(cent_clean,mapping=aes(geometry=geometry),fill=NA,pch=21,size=0.5)+
  labs(fill="Member of a sport association")+
  guides(
    fill=guide_legend(
      nrow=1,
      title.position="top",
      label.position="bottom"
    )
  )+
  scale_fill_manual(
    values=pal,
    label=c("< 18 %","< 20 %","< 22 %","< 24 %","< 26 %", "≥ 26 %")
  )+
  theme_custom


grd_clean_fish <- sf_fisheye(grd_clean)
cent_clean_fish <- sf_fisheye(cent_clean)

ggplot()+
  geom_sf(
    # drop_na() is one way to suppress the cells outside the country
    grd_clean_fish%>%drop_na(), 
    mapping=aes(geometry=geometry,fill=clss)
  )+
  geom_sf(cent_clean_fish,mapping=aes(geometry=geometry),fill=NA,pch=21,size=0.5)+
  labs(fill="Member of a sport association")+
  guides(
    fill=guide_legend(
      nrow=1,
      title.position="top",
      label.position="bottom"
    )
  )+
  scale_fill_manual(
    values=pal,
    label=c("< 18 %","< 20 %","< 22 %","< 24 %","< 26 %", "≥ 26 %")
  )+
  theme_custom

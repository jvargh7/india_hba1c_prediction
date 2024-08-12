rm(list=ls());gc();source(".Rprofile")

library(sf)
library(tmap)

path_india_shapefiles <- "C:/Cloud/OneDrive - Emory University/data/India Shapefiles/"

nfhs5_district_estimates <- read_csv("analysis/ihpan02_nfhs5 district estimates.csv") %>% 
  dplyr::filter(n >=30) %>% 
  mutate(dm_control_hba1c = dm_control_hba1c*100,
         dm_control_rbg = dm_control_rbg*100)

district_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/dnfhs5_sp.RDS"))
state_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/smapsmaster_sp.RDS"))

map_shp <- district_shp %>% 
  sp::merge(nfhs5_district_estimates %>% 
              dplyr::select(district,dm_control_hba1c,dm_control_rbg),
            by.x="REGCODE",by.y="district",all.x=TRUE)

figB <- tm_shape(map_shp,ext=1.2) + 
  tm_fill(title= "Control (%)",
          col="dm_control_hba1c",
          palette="RdYlGn",
          style = "fixed",
          breaks= seq(0,100,by=20),
          # midpoint = NA,
          textNA="<30 observations",
          colorNA = "white")+ 
  tm_borders() + 
  tm_shape(state_shp) + tm_borders(col="black") + 
  tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE) +
  tm_legend(legend.position = c("right","top"),
            legend.outside=FALSE,
            legend.just=c("left","top"))+ 
  tm_xlab("") +
  tm_ylab("") +
  tm_layout("B",title.size = 2,
            legend.text.size = 1,
            legend.title.size = 1)


figA <- tm_shape(map_shp,ext=1.2) + 
  tm_fill(title= "Control (%)",
          col="dm_control_rbg",
          palette="RdYlGn",
          style = "fixed",
          breaks= seq(0,100,by=20),
          # midpoint = NA,
          textNA="<30 observations",
          colorNA = "white")+ 
  tm_borders() + 
  tm_shape(state_shp) + tm_borders(col="black") + 
  tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE) +
  tm_legend(legend.position = c("right","top"),
            legend.outside=FALSE,
            legend.just=c("left","top"))+ 
  tm_xlab("") +
  tm_ylab("") +
  tm_layout("A",title.size = 2,
            legend.text.size = 1,
            legend.title.size = 1)

tmap_arrange(
  figA,figB,
  ncol = 2,nrow=1) %>% 
  tmap_save(.,filename=paste0(path_india_hba1c_box_folder,"/figures/national map control.jpg"),height=7,width=14)

library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggnewscale)
library(whitebox)
library(leaflet)
#all of these layers need to be in the same projection and read in through the terra package 
get_upstream_area = function(dem, floodplain,watershed, out_path, overwrite = T ){
  #this if statement deletes all the files stored it the specified outpath
  if (overwrite == T){
    unlink(paste0(out_path,"*"))
  }
  #1. crop the dem to the extent of the specified watershed and save it to the out_path directory
  demcrop = crop(dem,watershed)
  writeRaster(demcrop, paste0(out_path,"dem.tif"))
  
  #2. whitebox tools flow acumulation full workflow input is the dem from step 1 the output is a filled dem, d8 pointer file and a flow accumulation file
  wbt_flow_accumulation_full_workflow(paste0(out_path,"dem.tif"),
                                      paste0(out_path,"filldem.tif"),
                                      paste0(out_path,"pointer.tif"),
                                      paste0(out_path,"accum.tif"))
  
  # 3. delineate the streams from the flow accumulation raster, with a contribution area of 10... small but it gets many streams, convect streams to points
  wbt_extract_streams( paste0(out_path,"accum.tif"), 
                       paste0(out_path,"streams.tif"),
                       10)
    wbt_raster_to_vector_points(paste0(out_path,"streams.tif"),
                              paste0(out_path,"streamspoints.shp"))
  
    
  #4. This step finds the lower elevation point inside the floodplain polygon  
  squamstreampoints = read_sf(paste0(out_path,"streamspoints.shp")) #read in the stream points as sf object
  
  vectpoint = vect(paste0(out_path,"streamspoints.shp"))# read in stream points as terra object
  floodpoints = terra::intersect(vectpoint,floodplain)
  streamelev = terra::extract(demcrop,floodpoints) # extract values from dem using the terra points 
  #floodpoints = left_join(floodpoints,streamelev, by = c("FID" ="ID")) # join elevation information and spatial information 
  floodpoints = cbind(floodpoints,streamelev)
  floodsf = st_as_sf(floodplain) #convert to sf object 
  floodpoints = st_as_sf(floodpoints)
  floodjoin = st_join(floodsf,floodpoints) #spatial join elevation to floodplain 
  
  highpoint = filter(floodpoints, elevation == min(floodjoin$elevation))%>% #filter for lower elevation point in the floodplain 
    filter(FID ==min(FID))
  st_write(highpoint,paste0(out_path,"highpoint2.shp"))#save lowest point 
  
  #use lowest point in watershed function 
  wbt_watershed(paste0(out_path,"pointer.tif"),
                paste0(out_path,"highpoint2.shp"),
                paste0(out_path,"watersheds2.tif"))
  #read in upstream area
  upstreamarea =  rast(paste0(out_path,"watersheds2.tif"))
  return(upstreamarea)#output of the function is a raster showing the upstream area
}

#create output file path 
out_path = "D:/WFI/Activity 4/_data/temppath/"
dir.create(out_path)

#Read dem, wateshed layer and floodplain layer 
dem = rast("./_data/bcdem.tif")
watersf = read_sf("D:/WFI/Activity 4/_data/FWA_NAMED_WATERSHEDS_POLY/FWNMDWTRSH_polygon.shp")

floodsf = read_sf("D:/WFI/Activity 4/_data/BC_floodplains/BC_floodplains.shp")%>% #select random floodplain and find center point 
  #sample_n(1)%>%
  filter(HydroID == 724540) %>%
  st_point_on_surface()

floodsf = st_join(floodsf,watersf)# determine which watershed floodplain belongs in 

floodsf = floodsf %>%
  filter(AREA_HA == min(AREA_HA)) # if it belongs to many watersheds select the smallest
print(floodsf$GNIS_NAME) #get the name of the watershed 


#read in,filter and project the watersheds and floodplains using terra  
floodplain = vect("D:/WFI/Activity 4/_data/BC_floodplains/BC_floodplains.shp")%>%
  #filter(HydroID == floodsf$HydroID)%>%
  filter(HydroID == 724540) %>%
  project(dem)
watersheds = read_sf("D:/WFI/Activity 4/_data/FWA_NAMED_WATERSHEDS_POLY/FWNMDWTRSH_polygon.shp")%>%
  filter(GNIS_NAME == floodsf$GNIS_NAME)%>%
  vect()%>%
  project(dem)



#map it
leaflet()%>%
  addTiles()%>%
  addPolygons(data = st_as_sf(watersheds), fillOpacity = 0, label = as.character(as.character(watersheds$GNIS_NAME)))%>%
  addPolygons(data = st_as_sf(floodplain), fillColor = "salmon", color = "salmon", label = as.character(floodplain$HydroID))%>%
  addLegend("topright", colors = c("blue", "salmon"), labels = c("Watershed", "Floodplains"))


#run the upstream area function 
chilco = get_upstream_area(dem,floodplain, watersheds,out_path)


#convert to polygon to map using leaflet
uparea = as.polygons(chilco)%>%
  st_as_sf()



leaflet()%>%
  addTiles()%>%
  addPolygons(data = st_as_sf(watersheds), fillOpacity = 0, label = as.character(as.character(watersheds$GNIS_NAME)))%>%
  addPolygons(data = st_as_sf(floodplain), fillColor = "salmon", color = "salmon", label = as.character(floodplain$HydroID))%>%
  addPolygons(data = uparea,fillColor = "purple", color = "purple")%>%
  addLegend("topright", colors = c("blue", "salmon", "purple"), labels = c("Watershed", "Floodplains", "Upstream Area"))










#this makes ggplot2 static map with dem 

demcrop = crop(dem, watersheds)
names(chilco)= "area"

levels(chilco) = data.frame(id = 1, area = c("Upstream Area"))

coldf = data.frame(value=1, col = c("skyblue4"))
coltab(chilco)= coldf

grad_hypso <- hypso.colors2(10, "dem_poster")


ggplot()+
  geom_spatraster(data = demcrop, alpha = 0.95)+
  scale_fill_gradientn(colours = grad_hypso ,na.value = NA)+
  labs(fill = "Elevation")+
  new_scale_fill()+
  geom_spatvector(data = watersheds,aes(colour = "watershed"), fill = NA )+
  geom_spatvector(data = floodplane, aes(color = "floodplane"), fill = "purple")+
  scale_color_manual(values = c("forestgreen","black"))+
  geom_spatraster(data = chilco, alpha = 0.5)+
  scale_fill_coltab(data = chilco)+
  labs(fill = "Upstream Area")+
  theme_bw()











# Order by altitude and river
# Add MB classification legend


#library(data.table)
#library(sf)
#library(raster)
library(dplyr)
library(exactextractr)
#library(rgdal)
library(leaflet)
library(htmltools)

library(rgrass)
library(terra)
library(sf)
#library(sp)
#library(rgdal)retired
#library(rgeos)retired
#library(maptools)retired

#setwd("C:/privateRlab/catchment")
#setwd("C:/Users/CastonT/Environment Protection Authority Victoria/Environmental Public Health Branch (internal) SharePoint - Documents/EHTN/R_lab/catchment")

generateGRASSshapeslines = TRUE
aggregate_raster_factor = 1 #needs to be adjusted to enlarge cells and avoid flow direction of < 1. 5.2 too small
#log_accumulation = x #this approach doesn't work for the execGRASS command
#line 86 stream_thresh <- log_accum>5.2  #this is an accumulation threshold based on the number of upstream raster cells
#%#%#% higher resolution from lower aggregate_raster_factor gives more streams, necessitating down adjustment of the
#log_accum< value to avoid convoluted stream modelling
## @ _factor = 5.5, log accum=4.5 fails for upper reaches of Yarra river(optimal for yarra: factor = 5.5, log_accum = 4.0)
##for McAlister: factor =6, log_accum >4.5; works for all but sample site 6(fact=6, log_accum>5.5)tried1;6.5nup,1;8.5nup,1:9nup
##For Bunyip Factor 5.5, log_accum > 3.0 (5.5; 3.5 nup; 5.5, 3.0 almost; 6, 4.0 nup) sites20,21,22,24 log_accum > 2?
##Paterson Factor 6, log accum>4.2 (5.5, 3.0 nup; 6, 4.0 nup) probably good for sample 12; beach
##But Frankston samples 13-15 are on little creeks factor 2, log_accum>2
##But site 14; factor 3, log_accum>1. Had to use the most stream.shp to get enough creek but with factor 3 for the basin calc

if(generateGRASSshapeslines){source("R/generateGRASSshapeslines.R")
} else {G <- initGRASS(gisBase="C:/Program Files/GRASS GIS 8.0", gisDbase="grassdata",location="drainage",mapset="PERMANENT", override=TRUE)
raster("dem.tif") -> rr
lgashape <- readOGR("C:/privateRlab/catchment/Vic_State_SA2.shp")
vicshape <- aggregate(lgashape, by = "STE_NAME17", do_union = TRUE, simplify = TRUE, join = st_intersects)
elevation_vic <- exactextractr::exact_extract(rr, vicshape, fun = NULL, include_cell = TRUE,include_xy = TRUE,force_df = TRUE, stack_apply = FALSE)
elevation_dat <- as.data.frame(elevation_vic)
rel <- rasterFromXYZ(as.data.frame(elevation_dat)[, c("x", "y", "value")])
crs(rel) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#crs(rel) <- "+init=epsg:4326"
re <- aggregate(rel, fact = aggregate_raster_factor, fun = "mean")
crs(re) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(re) <- "+init=epsg:4326"
}

#MESHBLOCKS__________________

sf_mb16 <- terra::vect("data/spatial/mb_2016_vic/MB_2016_VIC.shp") #saved in environment to sav time
#st_crs(sf_mb16)
###get centroids of meshblocks
#class(sf_mb16)
cents <- centroids(sf_mb16)

cents <- SpatialPointsDataFrame(coords=cents, data=sf_mb16@data,
                                proj4string=CRS("+proj=longlat +ellps=WGS84 +init=epsg:4326"))
#proj4string(cents)<- CRS("")

st_mb16 <- st_read("data/spatial/mb_2016_vic/MB_2016_VIC.shp") #saved in environment to sav time
#st_mb16 <- as.data.table(st_mb16)

#MESHBLOCK POPULATION and LANDUSE________________

abs_mb_pops <- read.csv("data/2016_census_mesh_block_counts_no_footer.csv",
                        colClasses = c("MB_CODE_2016" = "character"))
names(abs_mb_pops)[1] <- "MB_CODE16"


#SAMPLE_SITES_

##select catchment by grid point from sample sites table

points <- read.csv("data/pathogen_sample_sites.csv")
#points <- as.data.table(points)
#setnames(points$X, points$site_n)

points$site_n  <- as.numeric(seq.int(along.with = points$X))
#points <- points[37:43, ]
points <- points[, c("longitude", "latitude", "site_n")]

##snap points to lines

#outlet_coords  <- as.data.frame(coordinates(points))
#outlet_coords <- SpatialPoints(coords=outlet_coords,
                              # proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
#proj4string(outlet_coords)<- CRS("")("+proj=longlat +ellps=WGS84 +init=epsg:4326"))
outlet_coords <- st_as_sf(points, coords = c("longitude", "latitude"), crs = st_crs(4326))
#c <- st_as_sf(outlet_coords)
if(interactive()) mapview::mapview(outlet_coords)

#####Bring in streams spatial lines dataframe from the GRASS commands
s_vic <- st_read("data/spatial/streams/streams_temp.shp")
if(interactive()) mapview::mapview(s_vic)
 #d <- st_as_sf(s_vic)
# shapefile("streams.shp") -> s_vic
# plot(s_vic)
#snap_points <- snapPointsToLines(outlet_coords, s_vic, maxDist=NA, withAttrs = FALSE, idField=NA)
#snap_points <- st_snap(c, d)

#outlets <- as.data.frame(outlet_coords)
outlets <- outler
# setnames(outlets, "X", "Longitude")
# setnames(outlets, "Y", "Latitude")
#outlets$site_n  <- as.numeric(seq.int(nrow(outlets)))
#outlets <- cbind(outlets, points)
#by = c("site_n")
plot(st_geometry(s_vic))
plot(st_geometry(outlet_coords), add=T, col="blue")
#plot(snap_points, add=T)

###CADASTRAL LANDUSE_DATA
###make cadastral centroid coordinates list.
relist <- TRUE
if(relist){
  sf_lu <- st_read("data/spatial/landuse/LANDUSE_2017.shp")
  sf_lu$X <-  as.numeric(seq.int(nrow(sf_lu)))

 # st_crs(sf_lu)
  ###get centroids of cadastral polygons
  #class(sf_lu)
  cents_lu <- st_centroid(sf_lu)
  cents_lu$X <-    sf_lu$X

 # cents_lu <- as.data.table(cents_lu)
#  cents_lu$X  <- as.numeric(seq.int(nrow(cents_lu)))
#  cents_lu <- SpatialPointsDataFrame(coords=cents_lu, data=sf_lu@data,
#                                     proj4string=CRS("+proj=longlat +ellps=WGS84 +init=epsg:4326"))
#st_lu <- st_read("LANDUSE_2017.shp") #saved in environment to save time
#st_lu1 <- as.data.table(st_lu)
#st_lu1$X  <- as.numeric(seq.int(nrow(st_lu1)))

  }

##read cadastral land use file as table






##MAKE TABLE OF UPSTREAM PREDICTORS

###subset meshblock populations and landuse classifications to the list of upstream catchment meshblock names from centroids above sampl site altitudes


site_todo <- unique(outlets$site_n)

for(i in 1:length(site_todo)){
 i = 30
  i <- site_todo[i]

 ###&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  outlet_coord <- outlets[outlets$site_n == i, ]
  #outlet_coord <- outlet_coord[ , c("longitude", "latitude") ]
  #outlet_coord <- coordinates(outlet_coord)
 coords <- st_coordinates(outlet_coord$geometry) |> as.vector()

  execGRASS("r.water.outlet", flags = 'overwrite',
            parameters = list(input = 'fdir_temp', output="basin_A31",   # flow direction as input
                              coordinates = coords))

  execGRASS('r.to.vect', flags='overwrite',
            parameters = list(input='basin_A31',
                              output='catchment', type="area"))
  execGRASS('v.out.ogr', flags=c('overwrite'),
            parameters=list(input='catchment',
                            output="basin.shp",type="area",
                            format="ESRI_Shapefile"))

  ##&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  #read the results and plot:

  #shapefile("basin.shp") -> b
  st_read("basin.shp") -> b
  #shapefile("area.shp") -> a
  #shapefile("flows.shp") -> f
  plot(b$geometry)


  ##make elevation raster
  elevation_b <- exactextractr::exact_extract(rel, b, fun = NULL, include_cell = TRUE,include_xy = TRUE,force_df = TRUE, stack_apply = FALSE)
  elevation_b <- as.data.frame(elevation_b)[, c("x", "y", "value")]
  r <- terra::rast(elevation_b, crs = st_crs(b))


  plot(r)
  plot(b, add=T)
  plot(s_vic, add=T, col="red")
  plot(outlet_coords, add=T, col="blue")
  #plot(snap_points, add=T)


   ###subset cadastral spatial polygons dataframe to the catchment
  subcatch_lu <- cents_lu[b,]
  lu <- unique(subcatch_lu$X, na.rm=T)

  st_lu17 <- st_lu1[X %in% lu]
  st_lu17 <- as.data.table(st_lu17)
  st_lu17 <- st_lu17[!LU_DESCR_A == "Urban residential", ]
  qc1 <- sum(st_lu17$HECTARES)
  qc2 <- sum(st_lu1$HECTARES)

  #st_lu17 <- na.omit(st_lu17)

  subcatch <- cents[b,]
  mb <- unique(subcatch$MB_CODE16, na.rm=T)

  st_mb_pop <- merge(st_mb16, abs_mb_pops, by = c("MB_CODE16"))
  st_mb_sub <- st_mb_pop[MB_CODE16 %in% mb]
  qc3 <- sum(st_mb_sub$AREASQKM16*100)
  res <- c("Residential", "Commercial", "Education", "Industrial")
  st_mb161 <- st_mb_sub[MB_CATEGORY_NAME_2016 %in% res]
  #st_mb161 <- na.omit(st_mb_161)
  qc4 <- sum(st_mb161$AREASQKM16*100)


  ####make table of MB and ALUIS attributes
  area_lu <- st_lu17[, c("PARCEL_PFI", "HECTARES", "LU_DESCR_A")]
  summary_lu <- area_lu[,.(Area = sum(HECTARES)), by = c("LU_DESCR_A")]
  summary_lu$lu_record <- "VLUIS"
  #summary_lu$area_prop <- round(summary_lu$Area/sum(summary_lu$Area), 6)
  area_mb <- st_mb161[, c("MB_CODE16", "AREA_ALBERS_SQKM", "MB_CATEGORY_NAME_2016")]
  area_mb <-  as.data.table(area_mb)
  area_mb <- area_mb[, .(PARCEL_PFI = MB_CODE16, HECTARES = AREA_ALBERS_SQKM*100, LU_DESCR_A = MB_CATEGORY_NAME_2016)]
  summary_mb <- area_mb[,.(Area = sum(HECTARES)), by = c("LU_DESCR_A")]
  summary_mb$lu_record <- "ABS"
  #summary_mb$area_prop <- round(summary_mb$Area/sum(summary_mb$Area), 6)
  area <- rbind(summary_lu, summary_mb, fill=TRUE)
  area <- as.data.table(area)
  area$area_prop <- round(area$Area/sum(area$Area), 6)
  qc5 <- sum(area_lu$HECTARES)
  qc6 <- sum(area_mb$HECTARES)

  #summary_land <- area[,.(Area = sum(HECTARES)), by = c("LU_DESCR_A")]
  summary_land <- area[,.(LU_DESCR_A, lu_record, Area, area_prop, site_n = i)]

  summary_land_pop <- st_mb_sub[,.(population = sum(Person), Area = sum(AREA_ALBERS_SQKM*100), Dwellings = sum(Dwelling)), by = c("MB_CATEGORY_NAME_2016")]
  summary_landuse <- summary_land_pop[,.(MB_CATEGORY_NAME_2016, population, Area, Dwellings, area_prop = round(Area/sum(Area), 2), site_n = i)]



  if(i == 41){
    summary_vluis_abs <- summary_land
  } else {
    summary_vluis_abs <- rbind(summary_vluis_abs, summary_land, fill = T)
  }

  if(i == 41){
    summary_landuse1 <- summary_landuse
  } else {
    summary_landuse1 <- rbind(summary_landuse1, summary_landuse, fill = T)
  }

  # view <- setorder(summary_landuse1, MB_CATEGORY_NAME_2016)
  # view
}
#all_meshblocks <- merge(summary_landuse1, sample_sites, by = c("site_n"))
#qc1 <- unique(all_data)

write.csv(summary_vluis_abs, "summary_lu_mb_yarra.csv")
write.csv(summary_landuse1, "summary_mb_pop_yarra.csv")


#_____________________________maps___________________________________
st_lu17v1 <- as.data.frame(st_lu17)
attr <- st_lu17v1
st_lu17v1$geometry <- as(st_lu17v1$geometry, "Spatial")
t <- as(st_lu17v1$geometry, "SpatialPolygons")
st_lu17v2 <- as(t, "SpatialPolygonsDataFrame")
####restore attributes to spdf of catchment meshblocks
st_lu17v2@data <- attr[]
#plot(st_lu17v2)

st_mb16v1 <- as.data.frame(st_mb161)
attr <- st_mb16v1
st_mb16v1$geometry <- as(st_mb16v1$geometry, "Spatial")
e <- as(st_mb16v1$geometry, "SpatialPolygons")
st_mb16v2 <- as(e, "SpatialPolygonsDataFrame")
####restore attributes to spdf of catchment meshblocks
st_mb16v2@data <- attr
#plot(st_mb16v2)
###load street map
map <- leaflet()
map <- addTiles(map, urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", layerId = NULL, group = NULL, data = getMapData(map)) %>%
  fitBounds(144.5, -38.5, 146, -37)
map
#s <- readOGR("streams.shp")
s <- s_vic

projectRasterForLeaflet(r, method = c("ngb"))
v <- elevation_dat$value
colors = colorNumeric(c("#0000FF", "#FFA500", "#8B0000"), domain = v , na.color = "transparent")

mylabels <- paste(
  "Meshblock: ", st_mb16v2$MB_CODE16, sep = "<br/>",
  "landuse: ", st_mb16v2$MB_CATEGORY_NAME_2016,
  "Population: ", st_mb16v2$Person,
  # "Altitude: ", st_mb16v2$mb_alt,
  # "latitude:", st_mb16v2$lat,
  # "longitude:", st_mb16v2$lon,
  ecollapse = NULL, recycle0 = FALSE

) %>%
  lapply(htmltools::HTML)


mylabels_lu <- paste(
  "Parcel: ", st_lu17v2$PARCEL_PFI, sep = "<br/>",
  "landuse: ", st_lu17v2$LU_DESCR_A,
  "landuse1:", st_lu17v2$LU_DESC,
  # "Altitude: ", st_mb16v2$mb_alt,
  # "latitude:", st_mb16v2$lat,
  # "longitude:", st_mb16v2$lon,
  ecollapse = NULL, recycle0 = FALSE

) %>%
  lapply(htmltools::HTML)


mylabels1 <- paste(
  "Site: ", outlets$site_n, sep = "<br/>",
  # "Date: ", sample_sites$Date,
  # "Contaminant: ", sample_sites$Contaminant,
  # "Concentration: ", sample_sites$Formatted_value,
  #"Altitude: ", sample_sites$alt_points,
  "latitude:", outlets$latitude,
  "longitude:", outlets$longitude,
  ecollapse = NULL, recycle0 = FALSE

) %>%
  lapply(htmltools::HTML)

x <- st_mb16v2$MB_CATEGORY_NAME_2016
pal <- colorFactor(c("#0000FF", "#FFA500", "#8B0000"), domain = x,
                   na.color = "transparent")

y <- st_lu17v2$LU_DESCR_A
palu <- colorFactor(c("#709302", "#eaaf0f", "#dd7a00", "#d81e05", "#9b301c"), domain = y,
                   na.color = "transparent")

map <- leaflet(outlets) %>%
  # Base groups
  addTiles(map, urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", layerId = NULL, group = NULL, data = getMapData(map)) %>%
  # Overlay groups
  addCircleMarkers(~longitude, ~latitude, color = "black", label = mylabels1, group = "Sample sites") %>%

  addRasterImage(r, colors = colors, opacity = 0.8, group = "Elevation") %>%
  #
  addPolylines(data = s, color = "red", weight = 1, group = "Streams") %>%

  addPolygons(data = st_mb16v2,
              stroke = TRUE,
              fillColor = ~pal(st_mb16v2$MB_CATEGORY_NAME_2016),
              fillOpacity = 0.6,
              color = 'White',
              weight = 1.5,
              label = mylabels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"), group = "MB polygons"  ) %>%

  addPolygons(data = st_lu17v2,
              stroke = TRUE,
              fillColor = ~palu(st_lu17v2$LU_DESCR_A),
              fillOpacity = 0.6,
              color = 'White',
              weight = 1.5,
              label = mylabels_lu,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"), group = "VLUIS polygons"  ) %>%
  #Layers control
  addLayersControl(

    overlayGroups = c("Sample sites", "Elevation", "Streams", "MB polygons", "VLUIS polygons"),
    options = layersControlOptions(collapsed = TRUE)
) %>%
addLegend(position = c("topleft"), pal = colors, values = v, group = "Elevation",
          title = "Elevation (m)"
) %>%
addLegend(position = c("topleft"), pal = pal, values = x, group = "MB polygons",
            title = "Land Use"
) %>%
addLegend(position = c("bottomleft"), pal = palu, values = y, group = "VLUIS polygons",
            title = "Land Use"
) %>%
  addScaleBar(
  position = c("bottomleft"), options = scaleBarOptions(imperial = FALSE,
                                                         updateWhenIdle = TRUE)
  )
map


library(data.table)
library(sf)
library(raster)
library(dplyr)
library(exactextractr)
library(rgdal)
library(leaflet)
library(htmltools)
#library(GISTools)
library(rgrass)
library(sp)
library(rgdal)
#library(rgeos)
library(maptools)


#SHAPE catchments____________________________
###make catchment shapefile and flow direction layer from dynamic elevation model
#setwd("C:/Documents/catchment")

raster("dem.tif") -> rr
rr <- setMinMax(rr)

#rr[rr <= 0] <- NA
wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
utm <- "+proj=utm +zone=48 +south=T ellps=WGS84"
name <- names(rr)
lgashape <- readOGR("C:/privateRlab/catchment/Vic_State_SA2.shp")
vicshape <- aggregate(lgashape, by = "STE_NAME17", do_union = TRUE, simplify = TRUE, join = st_intersects)
st_crs(lgashape)

elevation_vic <- exactextractr::exact_extract(rr, vicshape, fun = NULL, include_cell = TRUE,include_xy = TRUE,force_df = TRUE, stack_apply = FALSE)

elevation_dat <- as.data.frame(elevation_vic)
# max(elevation_dat$value, na.rm=T)

G <- initGRASS(gisBase="C:/Program Files/GRASS GIS 8.0",  gisDbase="grassdata",location="drainage",mapset="PERMANENT", override=TRUE)
#home = "C:/privateRlab/catchment/GRASS GIS 8.0/bin",
rel <- rasterFromXYZ(as.data.frame(elevation_dat)[, c("x", "y", "value")])
re <- aggregate(rel, fact = aggregate_raster_factor, fun = "mean")
crs(re) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(re) <- "+init=epsg:4326"
plot(re)
st_crs(re)
crs(rel) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(rel) <- "+init=epsg:4326"

#,
#set the region and covert raster to be spatial grid data frame
rast <- as(re, "SpatialGridDataFrame")
proj4string(rast)<- CRS("+init=epsg:4326")
write_RAST(rast, "rast_img", flags = c("overwrite"))
execGRASS("r.info", map = "rast_img")
execGRASS("g.proj", flags = "c", epsg = 4326)


#set the region based on the mapset

execGRASS("g.region", raster = "rast_img")


str(rast)
image(rast, "value", col = terrain.colors(20))

# out_raster <- read_RAST("rast_img")
# #str(out_raster) #optional
# st_crs(out_raster)
# plot(out_raster)

#Run r.watershed, fdir_temp is flow direction, accum is flow accumulation, change the threshold with any integer values if needed.
# execGRASS("r.watershed", flags=c("overwrite", "a"),
#           parameters=list(elevation="rast_img", threshold=5000,
#                           drainage= "fdir_temp",stream="upstream", accumulation="accum", basin="rbasin"))

execGRASS("r.fill.dir", flags = c("overwrite"), parameters = list(input = "rast_img", output = "rast_nodep", direction = "fdir_temp_nodep"))
#execGRASS("r.lake", flags = c("overwrite"), parameters = list(elevation = "rast_img", lake = "rast_nodep", water_level="float"))
execGRASS("g.region", raster = "rast_nodep")
out_raster1 <- read_RAST("rast_nodep")

execGRASS("r.watershed", flags=c("overwrite", "a"),
          parameters=list(elevation="rast_nodep", threshold=5000,
                          drainage= "fdir_temp",stream="upstream", accumulation="accum", basin="rbasin"))


#run mapcalc (map calculator) to post process the flow accumulation into a stream
execGRASS("r.mapcalc",flags="overwrite", expression="log_accum=log(abs(accum)+1)")
execGRASS("r.mapcalc",flags="overwrite", expression="inf_rivers=if(log_accum>6.5)")

#Thin the raster to convert it as vector
execGRASS("r.null", map="inf_rivers", setnull = "0")
execGRASS('r.thin', flags='overwrite',
          parameters = list(input='inf_rivers',
                            output='riv_thin'))

#convert to vector and then save as shapefile
#thinned rivers
execGRASS("r.to.vect", flags='overwrite',
          parameters = list(input="riv_thin",
                            output="streams", type="line"))
execGRASS('v.out.ogr',flags=c('overwrite'),
          parameters=list(input='streams',
                          output="streams_temp.shp",type="line",
                          format="ESRI_Shapefile"))

##all catchments
execGRASS('r.to.vect', flags='overwrite',
          parameters = list(input='rbasin',
                            output='catchments', type="area"))
execGRASS('v.out.ogr', flags=c('overwrite'),
          parameters=list(input='catchments',
                          output="area.shp",type="area",
                          format="ESRI_Shapefile"))

##flow volume raster
execGRASS("r.to.vect", flags='overwrite',
          parameters = list(input="inf_rivers",
                            output="flows", type="area"))
execGRASS('v.out.ogr',flags=c('overwrite'),
          parameters=list(input='flows',
                          output="flows.shp",type="area",
                          format="ESRI_Shapefile"))

shapefile("streams_temp.shp") -> s_vic
plot(s_vic)

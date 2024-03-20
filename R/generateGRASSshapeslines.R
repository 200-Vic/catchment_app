
library(sf)
library(exactextractr)
library(rgrass)
library(terra)
library(fs)

#crete data directories if they do not exist
#data directories are .gitignored and so won't
#be available if this project has just been cloned from a git remote
#(currently Azure devOps)
dir_create("data/spatial/elevation_models")
dir_create("data/spatial/vic_sa2")
dir_create("data/spatial/streams")
dir_create("data/spatial/flows")
dir_create("data/spatial/area")

#SHAPE catchments____________________________
###make catchment shapefile and flow direction layer from dynamic elevation model

terra::rast("data/spatial/elevation_models/dem.tif") -> rr
rr <- setMinMax(rr)

#rr[rr <= 0] <- NA

lgashape <- terra::vect("data/spatial/vic_sa2/Vic_State_SA2.shp")

vicshape <- terra::aggregate(lgashape, by = "STE_NAME17") |>
  sf::st_as_sf() |>
  st_transform(crs(rr))



elevation_vic <- exactextractr::exact_extract(rr, vicshape, fun = NULL, include_cell = TRUE,include_xy = TRUE,force_df = TRUE, stack_apply = FALSE)

elevation_dat <- as.data.frame(elevation_vic)[, c("x", "y", "value")]

rel <- terra::rast(elevation_dat, crs = crs(rr))

aggregate_raster_factor <- 1
re <- terra::aggregate(rel, fact = aggregate_raster_factor, fun = "mean")

re <- project(re, "epsg:4326")



G <- initGRASS(gisBase="C:/Program Files/GRASS GIS 8.0",
               mapset = "PERMANENT",
               override=TRUE)



#,
#set the projection to epsg:4326
# gRASS need to be initialised with mapset = "PERMANENT"
# in order to change the projection
# see https://gis.stackexchange.com/a/230550
#
execGRASS("g.proj", flags = "c", epsg = 4326)

#cehck mapset and region settings
execGRASS("g.mapset", flags = "p")
execGRASS("g.region", flags = "p")

write_RAST(re, "rast_img", flags = c("overwrite"))
execGRASS("r.info", map = "rast_img")



#set the region based on the mapset

execGRASS("g.region", raster = "rast_img")


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
                          output="data/spatial/streams/streams_temp.shp",type="line",
                          format="ESRI_Shapefile"))

##all catchments
execGRASS('r.to.vect', flags='overwrite',
          parameters = list(input='rbasin',
                            output='catchments', type="area"))
execGRASS('v.out.ogr', flags=c('overwrite'),
          parameters=list(input='catchments',
                          output="data/spatial/area/area.shp",type="area",
                          format="ESRI_Shapefile"))

##flow volume raster
execGRASS("r.to.vect", flags='overwrite',
          parameters = list(input="inf_rivers",
                            output="flows", type="area"))
execGRASS('v.out.ogr',flags=c('overwrite'),
          parameters=list(input='flows',
                          output="data/spatial/flows/flows.shp",type="area",
                          format="ESRI_Shapefile"))

s_vic <- st_read("data/spatial/streams/streams_temp.shp")
plot(st_geometry(s_vic))

require(rgeos)
require(rgdal) # must be loaded after rgeos! reversed sequence leads to a full crach
require(ggplot2)
require(geosphere)
require(dplyr)
require(tikzDevice)
require(raster)
require(foreign)
require(sp)
require(rworldmap)
require(viridis)
require(GGally)
require(ggrepel)

project_data <-  function(
  df,  # a dataframe with Longitude, Latitude, and  data
  base = "~/Documents/110m_physical", # a base map file path
  base_layer = "ne_110m_land", # name of the layer in the base
  projection = "+proj=eqearth +wktext"
) {
  
  #### Settings:
  proj_setting <- paste(projection, "+lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
  #### Base map
  world.sp <- readOGR(dsn = base, layer = base_layer, verbose = F)
  
  # shift central/prime meridian towards west - positive values only
  shift <- 180 + 30
  # create "split line" to split polygons
  WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  split.line = SpatialLines(list(Lines(list(Line(cbind(180 - shift,c(-90,90)))), ID = "line")),
                            proj4string = WGS84)
  
  # intersect line with country polygons
  line.gInt <- gIntersection(split.line, world.sp)
  
  # create a very thin polygon (buffer) out of the intersecting "split line"
  bf <- suppressWarnings(gBuffer(line.gInt, byid = TRUE, width = 0.000001))
  
  # split country polygons using intersecting thin polygon (buffer)
  world.split <- gDifference(world.sp, bf, byid = TRUE)
  
  # transform split country polygons in a data table that ggplot can use
  world.sh.tr.df <- fortify(world.split)
  
  # Shift coordinates
  world.sh.tr.df$long.new <- world.sh.tr.df$long + shift
  world.sh.tr.df$long.new <- ifelse(world.sh.tr.df$long.new  > 180,
                                    world.sh.tr.df$long.new - 360, world.sh.tr.df$long.new)
  world.sh.tr.df[,c('X', 'Y')]  <- project(cbind(world.sh.tr.df$long.new, world.sh.tr.df$lat),
                                           proj = proj_setting)
  
  base_map.df <- subset(world.sh.tr.df, lat > -60 & lat < 85)
  
  #### Graticules
  b.box <- as(raster::extent(-180, 180, -90, 90), "SpatialPolygons")
  
  # assign CRS to box
  proj4string(b.box) <- WGS84
  
  # create graticules/grid lines from box
  grid <- gridlines(b.box,
                    easts  = seq(from = -180, to = 180, by = 20),
                    norths = seq(from = -90,  to = 90,  by = 10))
  
  # transform graticules from SpatialLines to a data frame that ggplot can use
  grid.df <- fortify(SpatialLinesDataFrame(sl = grid, data = data.frame(1:length(grid)),
                                           match.ID = FALSE))
  # assign matrix of projected coordinates as two columns in data table
  grid.df[, c("X","Y")]  <- project(cbind(grid.df$long, grid.df$lat),
                                    proj = gsub("lon_0=0", "lon_0=150", proj_setting))
  
  graticules.df <- subset(grid.df, lat > -60 & lat < 85)
  
  # create labels for graticules
  grid.lbl <- labels(grid, side = 1:4)
  
  # transform labels from SpatialPointsDataFrame to a data table that ggplot can use
  grid.lbl.df <- data.frame(grid.lbl@coords, grid.lbl@data)
  
  # add degree sign and clean up
  grid.lbl.df$labels <- ifelse(grepl("S|W", grid.lbl.df$labels),
                               paste0("-", gsub("\\*degree\\*?([EWSN])?", "", grid.lbl.df$labels), "°"),
                               paste0(gsub("\\*degree\\*?([EWSN])?", "", grid.lbl.df$labels), "°")
  )
  
  # grid.lbl.df$labels <- paste0(grid.lbl.df$labels,"°")
  # grid.lbl.df$labels <- gsub("\\*degree\\*?([EWSN])?", "", grid.lbl.df$labels, perl = T)
  
  # adjust coordinates of labels so that they fit inside the globe
  grid.lbl.df$long <- ifelse(grid.lbl.df$coords.x1 %in% c(-180,180),
                             grid.lbl.df$coords.x1*175/180, grid.lbl.df$coords.x1)
  
  grid.lbl.df$lat <-  ifelse(grid.lbl.df$coords.x2 %in% c(-90,90),
                             grid.lbl.df$coords.x2*60/90, grid.lbl.df$coords.x2)
  grid.lbl.df[, c("X","Y")] <-  project(cbind(grid.lbl.df$long, grid.lbl.df$lat),
                                        proj = gsub("lon_0=0", "lon_0=150", proj_setting))
  grid_label.df <- rbind(subset(grid.lbl.df, pos == 2 & coords.x2 > -70 &
                                  !labels %in% c("-60°", "90°")),
                         subset(grid.lbl.df, pos == 1 & !(abs(coords.x1) == 180 & coords.x2 == -90)))
  
  #### Data
  
  if (any(names(df) %in% c('longitude')) | any(names(df) %in% c('latitude'))) {
    df$Longitude <- df$longitude
    df$Latitude <- df$latitude
  }
  
  if (any(names(df) %in% c('lon')) | any(names(df) %in% c('lat'))) {
    df$Longitude <- df$lon
    df$Latitude <- df$lat
  }
  
  df.sp <- SpatialPointsDataFrame(df[,c("Longitude","Latitude")], df,
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  df.sp.tr <- spTransform(df.sp, CRS(gsub("lon_0=0", "lon_0=150", proj_setting)))
  
  data.df <- data.frame(df.sp.tr)
  
  data.df$X <- data.df$Longitude.1
  data.df$Y <- data.df$Latitude.1
  
  return(list(base_map = base_map.df,
              graticules = graticules.df,
              graticule_labels = grid_label.df,
              data = data.df))
  
}

### generate average metrics, UniNum

MDL <- read.csv('metrics/MDL_uninum.tsv',sep='\t')

LDLprod <- read.csv('metrics/LDL_production_uninum.tsv',sep='\t')
LDLprod <- aggregate(PER ~ language, LDLprod, FUN=mean)

LDLcomp <- read.csv('metrics/LDL_comprehension_uninum.tsv',sep='\t')
LDLcomp <- aggregate(accuracy ~ language, LDLcomp, FUN=mean)

surprisal <- read.csv('metrics/surprisals_uninum.tsv',sep='\t')
surprisal <- aggregate(surprisal ~ language, surprisal, FUN=mean)

uninum.merged <- merge(MDL, LDLprod, by='language')
uninum.merged <- merge(uninum.merged, LDLcomp, by='language')
uninum.merged <- merge(uninum.merged, surprisal, by='language')

metadata <- read.csv(url('https://raw.githubusercontent.com/numeralbank/googleuninum/master/cldf/languages.csv'))

uninum.merged <- merge(uninum.merged,metadata,by.x='language',by.y='ID')
uninum.merged <- na.omit(uninum.merged)

uninum.merged.PCA <- prcomp(uninum.merged[,c('surprisal','PER','MDL','accuracy')])

uninum.merged$PC1 <- uninum.merged.PCA$x[,1]

### plots, UniNum

uninum.df.g <- project_data(df = uninum.merged,
                            base = "~/Documents/Documents/110m_physical",
                            base_layer = "ne_110m_land",
                            projection = "+proj=eqearth +wktext"
                            # projection = "+proj=robin"
)

uninum.MDL.map <- ggplot() + theme_void() +
  geom_polygon(data = uninum.df.g$base_map,
               aes(x = X, y = Y, group = group),
               fill = 'lightgray',
               color = 'black',
               size = .1
  ) +
  geom_path(data = uninum.df.g$graticules,
            aes(x = X, y = Y, group = group),
            linetype = 'dotted',
            colour = 'grey',
            size = .25
  ) + 
  geom_text(data = uninum.df.g$graticule_labels,
            aes(x = X, y = Y, label = labels), size = 1.5, color = 'grey30') + 
  geom_point(data = uninum.df.g$data,
             aes(x = X, y = Y, color = MDL),
             alpha = .75,
             size = 2) + 
  scale_color_viridis()


uninum.PER.map <- ggplot() + theme_void() +
  geom_polygon(data = uninum.df.g$base_map,
               aes(x = X, y = Y, group = group),
               fill = 'lightgray',
               color = 'black',
               size = .1
  ) +
  geom_path(data = uninum.df.g$graticules,
            aes(x = X, y = Y, group = group),
            linetype = 'dotted',
            colour = 'grey',
            size = .25
  ) + 
  geom_text(data = uninum.df.g$graticule_labels,
            aes(x = X, y = Y, label = labels), size = 1.5, color = 'grey30') + 
  geom_point(data = uninum.df.g$data,
             aes(x = X, y = Y, color = PER),
             alpha = .75,
             size = 2) + 
  scale_color_viridis()


uninum.accuracy.map <- ggplot() + theme_void() +
  geom_polygon(data = uninum.df.g$base_map,
               aes(x = X, y = Y, group = group),
               fill = 'lightgray',
               color = 'black',
               size = .1
  ) +
  geom_path(data = uninum.df.g$graticules,
            aes(x = X, y = Y, group = group),
            linetype = 'dotted',
            colour = 'grey',
            size = .25
  ) + 
  geom_text(data = uninum.df.g$graticule_labels,
            aes(x = X, y = Y, label = labels), size = 1.5, color = 'grey30') + 
  geom_point(data = uninum.df.g$data,
             aes(x = X, y = Y, color = accuracy),
             alpha = .75,
             size = 2) + 
  scale_color_viridis()


uninum.surprisal.map <- ggplot() + theme_void() +
  geom_polygon(data = uninum.df.g$base_map,
               aes(x = X, y = Y, group = group),
               fill = 'lightgray',
               color = 'black',
               size = .1
  ) +
  geom_path(data = uninum.df.g$graticules,
            aes(x = X, y = Y, group = group),
            linetype = 'dotted',
            colour = 'grey',
            size = .25
  ) + 
  geom_text(data = uninum.df.g$graticule_labels,
            aes(x = X, y = Y, label = labels), size = 1.5, color = 'grey30') + 
  geom_point(data = uninum.df.g$data,
             aes(x = X, y = Y, color = surprisal),
             alpha = .75,
             size = 2) + 
  scale_color_viridis()


uninum.PC1.map <- ggplot() + theme_void() +
  geom_polygon(data = uninum.df.g$base_map,
               aes(x = X, y = Y, group = group),
               fill = 'lightgray',
               color = 'black',
               size = .1
  ) +
  geom_path(data = uninum.df.g$graticules,
            aes(x = X, y = Y, group = group),
            linetype = 'dotted',
            colour = 'grey',
            size = .25
  ) + 
  geom_text(data = uninum.df.g$graticule_labels,
            aes(x = X, y = Y, label = labels), size = 1.5, color = 'grey30') + 
  geom_point(data = uninum.df.g$data,
             aes(x = X, y = Y, color = PC1),
             alpha = .75,
             size = 2) + 
  scale_color_viridis()

tikz('graphics/PCA-map.tex',width=10,height=6)
ggplot() + theme_void() +
  geom_polygon(data = uninum.df.g$base_map,
               aes(x = X, y = Y, group = group),
               fill = 'lightgray',
               color = 'black',
               size = .1
  ) +
  geom_path(data = uninum.df.g$graticules,
            aes(x = X, y = Y, group = group),
            linetype = 'dotted',
            colour = 'grey',
            size = .25
  ) + 
  geom_text(data = uninum.df.g$graticule_labels,
            aes(x = X, y = Y, label = labels), size = 1.5, color = 'grey30') + 
  geom_point(data = uninum.df.g$data,
             aes(x = X, y = Y, color = PC1),
             alpha = .75,
             size = 2) + 
  scale_color_viridis()
dev.off()




###
###
### S. Asia metrics

MDL <- read.csv('metrics/MDL_sand.tsv',sep='\t')

LDLprod <- read.csv('metrics/LDL_production_sand.tsv',sep='\t')
LDLprod <- aggregate(PER ~ language, LDLprod, FUN=mean)

LDLcomp <- read.csv('metrics/LDL_comprehension_sand.tsv',sep='\t')
LDLcomp <- aggregate(accuracy ~ language, LDLcomp, FUN=mean)

surprisal <- read.csv('metrics/surprisals_sand.tsv',sep='\t')
surprisal <- aggregate(surprisal ~ language, surprisal, FUN=mean)

sand.merged <- merge(MDL, LDLprod, by='language')
sand.merged <- merge(sand.merged, LDLcomp, by='language')
sand.merged <- merge(sand.merged, surprisal, by='language')

metadata <- read.csv(url('https://raw.githubusercontent.com/numeralbank/sand/main/cldf/languages.csv'))

metadata <- rbind(metadata,read.csv('extra_data/languages.csv'))

metadata <- metadata[,c('ID','Name','Glottocode','Latitude','Longitude','Family','Base')]

#fix Brokskad, which is associated with Brokkad (TB, Bhutan), not Shina dialect
levels(metadata$Glottocode) <- c(levels(metadata$Glottocode),'brok1247')
metadata[metadata$ID == 'Brokskad',]$Glottocode <- 'brok1247'
metadata[metadata$ID == 'Brokskad',]$Latitude <- 34.53
metadata[metadata$ID == 'Brokskad',]$Longitude <- 76.60

sand.merged <- merge(sand.merged,metadata,by.x='language',by.y='ID')
sand.merged <- na.omit(sand.merged)

sand.merged.PCA <- prcomp(sand.merged[,c('surprisal','PER','MDL','accuracy')])

sand.merged$PC1 <- sand.merged.PCA$x[,1]

sand.merged[sand.merged$Family=='Tibeto-Burman',]$Family <- 'Sino-Tibetan'
sand.merged[sand.merged$Family=='Tibeto-Burman ',]$Family <- 'Sino-Tibetan'
sand.merged[sand.merged$Family=='Tibeto-Burman (Kiranti)',]$Family <- 'Sino-Tibetan'

sand.merged <- droplevels(sand.merged)

elevation_world <- getData('worldclim', var='alt', res=2.5)

sand.merged$elevation <- extract(elevation_world,sand.merged[,c('Longitude','Latitude')])

elev.p <- rasterToPoints(elevation_world)

elev.df <- data.frame(elev.p)

colnames(elev.df) = c("lon", "lat", "Altitude")

cbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

world <- map_data('world')

sand.merged$vigesimal <- grepl('Vigesimal',sand.merged$Base)

S.Asia.MDL.map <- ggplot() + 
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = NA, size = 0.1
  ) + 
  geom_point(data = sand.merged,
             aes(x = Longitude, y = Latitude, shape = Family, color = MDL),
             alpha = .5
  ) + xlim(50,100) + ylim(0,50) + scale_color_viridis()

S.Asia.PER.map <- ggplot() + 
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = NA, size = 0.1
  ) + 
  geom_point(data = sand.merged,
             aes(x = Longitude, y = Latitude, shape = Family, color = PER),
             alpha = .5
  ) + xlim(50,100) + ylim(0,50) + scale_color_viridis()

S.Asia.accuracy.map <- ggplot() + 
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = NA, size = 0.1
  ) + 
  geom_point(data = sand.merged,
             aes(x = Longitude, y = Latitude, shape = Family, color = accuracy),
             alpha = .5
  ) + xlim(50,100) + ylim(0,50) + scale_color_viridis()

S.Asia.surprisal.map <- ggplot() + 
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = NA, size = 0.1
  ) + 
  geom_point(data = sand.merged,
             aes(x = Longitude, y = Latitude, shape = Family, color = surprisal),
             alpha = .5
  ) + xlim(50,100) + ylim(0,50) + scale_color_viridis()


S.Asia.PC1.map <- ggplot() + 
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = NA, size = 0.1
  ) + 
  geom_point(data = sand.merged,
             aes(x = Longitude, y = Latitude, shape = Family, color = PC1),
             alpha = .5
  ) + xlim(50,100) + ylim(0,50) + scale_color_viridis()

tikz('graphics/S-Asia-map.tex',width=8*.75,height=6*.75)
S.Asia.PC1.map
dev.off()



IA.PC1.map <- ggplot() + 
  geom_raster(data=elev.df, aes(lon, lat, fill = Altitude), alpha=.3) + 
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = NA, size = 0.1
  ) + 
  geom_point(data = sand.merged[sand.merged$Family=='Indo-Aryan',],
             aes(x = Longitude, y = Latitude, color = vigesimal, size = PC1),
             alpha = .75
  ) + xlim(50,100) + ylim(0,50) + scale_colour_manual(values=cbPalette)

IA.PC1.map.labeled <- ggplot() + 
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = NA, size = 0.1
  ) + 
  geom_point(data = sand.merged[sand.merged$Family=='Indo-Aryan',],
             aes(x = Longitude, y = Latitude, shape = Family, fill = PC1),
             alpha = .5
  ) + 
  geom_label_repel(
    data = sand.merged[sand.merged$Family=='Indo-Aryan',],
    aes(x = Longitude, y = Latitude, label = Name, fill = PC1), size=2, max.overlaps = 1e10
  ) + 
  xlim(50,100) + ylim(0,50) + scale_fill_viridis()


elevation.plot <- ggplot(sand.merged[sand.merged$Family=='Indo-Aryan',],aes(x=elevation,y=PC1)) + geom_point(alpha=.8) + geom_smooth(method='loess')

vigesimal.plot <- ggplot(sand.merged[sand.merged$Family=='Indo-Aryan',],aes(x=vigesimal,y=PC1)) + geom_boxplot()

tikz('graphics/IA-map.tex',width=8*.75,height=6*.75)
IA.PC1.map
dev.off()

tikz('graphics/elevation-plot.tex',width=8*.5,height=6*.5)
elevation.plot
dev.off()

tikz('graphics/vigesimal-plot.tex',width=8*.5,height=6*.5)
vigesimal.plot
dev.off()


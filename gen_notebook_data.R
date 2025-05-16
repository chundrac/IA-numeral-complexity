require(ggplot2)
require(geosphere)
require(dplyr)
require(raster)
require(foreign)
require(sp)
require(rworldmap)
require(viridis)
require(GGally)
require(ggrepel)

world <- map_data('world')

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

uninum.MDL.map <- ggplot() + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "black", fill = NA, size = 0.1) + 
  geom_point(data = uninum.merged, aes(x = Longitude, y = Latitude, color = MDL), alpha = .75, size = 2) + scale_color_viridis()

uninum.PER.map <- ggplot() + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "black", fill = NA, size = 0.1) + 
  geom_point(data = uninum.merged, aes(x = Longitude, y = Latitude, color = PER), alpha = .75, size = 2) + scale_color_viridis()

uninum.accuracy.map <- ggplot() + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "black", fill = NA, size = 0.1) + 
  geom_point(data = uninum.merged, aes(x = Longitude, y = Latitude, color = accuracy), alpha = .75, size = 2) + scale_color_viridis()

uninum.surprisal.map <- ggplot() + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "black", fill = NA, size = 0.1) + 
  geom_point(data = uninum.merged, aes(x = Longitude, y = Latitude, color = surprisal), alpha = .75, size = 2) + scale_color_viridis()

uninum.PC1.map <- ggplot() + 
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "black", fill = NA, size = 0.1) + 
  geom_point(data = uninum.merged, aes(x = Longitude, y = Latitude, color = PC1), alpha = .75, size = 2) + scale_color_viridis()

uninum.PCA.correlations <- ggpairs(uninum.merged[,c('MDL','surprisal','PER','accuracy','PC1')])

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

sand.PCA.correlations <- ggpairs(sand.merged[,c('MDL','surprisal','PER','accuracy','PC1')])

###Relationship with cardinality

data.df <- read.csv('metrics/surprisals_uninum.tsv',sep='\t')

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data.df <- data.df[data.df$language=='hin'|data.df$language=='eng_us'|data.df$language=='jpn'|data.df$language=='guj'|data.df$language=='ben'|data.df$language=='uzb'|data.df$language=='hne',]

Language <- rep(NA,nrow(data.df))

Language[data.df$language=='hin'] <- 'Hindi'
Language[data.df$language=='ben'] <- 'Bengali'
Language[data.df$language=='guj'] <- 'Gujarati'
Language[data.df$language=='eng_us'] <- 'English'
Language[data.df$language=='jpn'] <- 'Japanese'
Language[data.df$language=='uzb'] <- 'Uzbek'
Language[data.df$language=='hne'] <- 'Nepali'

Group <- rep(NA,nrow(data.df))

Group[data.df$language=='hin'] <- 'Indo-Aryan'
Group[data.df$language=='ben'] <- 'Indo-Aryan'
Group[data.df$language=='guj'] <- 'Indo-Aryan'
Group[data.df$language=='eng_us'] <- 'non-Indo-Aryan'
Group[data.df$language=='jpn'] <- 'non-Indo-Aryan'
Group[data.df$language=='uzb'] <- 'non-Indo-Aryan'
Group[data.df$language=='hne'] <- 'Indo-Aryan'

data.df$Language <- Language
data.df$Group <- Group

surprisal.cardinality.plot <- ggplot(data=data.df,aes(x=number,y=surprisal,color=Language,linetype=Group)) + geom_point(alpha=.6) + geom_smooth(method='glm') + ylab('Surprisal') + xlab('Cardinality') + theme_bw() + scale_colour_manual(values=cbPalette)

#tikz('graphics/surprisal-cardinality-plot.tex',width=8,height=6)
#surprisal.cardinality.plot
#dev.off()

data.df <- read.csv('metrics/LDL_production_uninum.tsv',sep='\t')

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data.df <- data.df[data.df$language=='hin'|data.df$language=='eng_us'|data.df$language=='jpn'|data.df$language=='guj'|data.df$language=='ben'|data.df$language=='uzb'|data.df$language=='hne',]

Language <- rep(NA,nrow(data.df))

Language[data.df$language=='hin'] <- 'Hindi'
Language[data.df$language=='ben'] <- 'Bengali'
Language[data.df$language=='guj'] <- 'Gujarati'
Language[data.df$language=='eng_us'] <- 'English'
Language[data.df$language=='jpn'] <- 'Japanese'
Language[data.df$language=='uzb'] <- 'Uzbek'
Language[data.df$language=='hne'] <- 'Nepali'

Group <- rep(NA,nrow(data.df))

Group[data.df$language=='hin'] <- 'Indo-Aryan'
Group[data.df$language=='ben'] <- 'Indo-Aryan'
Group[data.df$language=='guj'] <- 'Indo-Aryan'
Group[data.df$language=='eng_us'] <- 'non-Indo-Aryan'
Group[data.df$language=='jpn'] <- 'non-Indo-Aryan'
Group[data.df$language=='uzb'] <- 'non-Indo-Aryan'
Group[data.df$language=='hne'] <- 'Indo-Aryan'

data.df$Language <- Language
data.df$Group <- Group

production.cardinality.plot <- ggplot(data=data.df,aes(x=number,y=PER,color=Language,linetype=Group)) + geom_point(alpha=.6) + geom_smooth(method='glm',method.args=list(family="binomial")) + ylab('Phoneme Error Rate') + xlab('Cardinality') + theme_bw() + scale_colour_manual(values=cbPalette)

#tikz('graphics/production-cardinality-plot.tex',width=8,height=6)
#production.cardinality.plot
#dev.off()

data.df <- read.csv('metrics/LDL_comprehension_uninum.tsv',sep='\t')

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data.df <- data.df[data.df$language=='hin'|data.df$language=='eng_us'|data.df$language=='jpn'|data.df$language=='guj'|data.df$language=='ben'|data.df$language=='uzb'|data.df$language=='hne',]

Language <- rep(NA,nrow(data.df))

Language[data.df$language=='hin'] <- 'Hindi'
Language[data.df$language=='ben'] <- 'Bengali'
Language[data.df$language=='guj'] <- 'Gujarati'
Language[data.df$language=='eng_us'] <- 'English'
Language[data.df$language=='jpn'] <- 'Japanese'
Language[data.df$language=='uzb'] <- 'Uzbek'
Language[data.df$language=='hne'] <- 'Nepali'

Group <- rep(NA,nrow(data.df))

Group[data.df$language=='hin'] <- 'Indo-Aryan'
Group[data.df$language=='ben'] <- 'Indo-Aryan'
Group[data.df$language=='guj'] <- 'Indo-Aryan'
Group[data.df$language=='eng_us'] <- 'non-Indo-Aryan'
Group[data.df$language=='jpn'] <- 'non-Indo-Aryan'
Group[data.df$language=='uzb'] <- 'non-Indo-Aryan'
Group[data.df$language=='hne'] <- 'Indo-Aryan'

data.df$Language <- Language
data.df$Group <- Group

comprehension.cardinality.plot <- ggplot(data=data.df,aes(x=number,y=accuracy,color=Language,linetype=Group)) + geom_point(alpha=.6) + geom_smooth(method='glm',method.args=list(family="binomial")) + ylab('Classification Accuracy') + xlab('Cardinality') + theme_bw() + scale_colour_manual(values=cbPalette)

#tikz('graphics/comprehension-cardinality-plot.tex',width=8,height=6)
#comprehension.cardinality.plot
#dev.off()

data.df <- read.csv('metrics/surprisals_sensitivity_uninum.tsv',sep='\t',header=F)

colnames(data.df) <- c(.0001,.001,.01,.1,1)

alpha.sensitivity.uninum <- ggpairs(data.df)

data.df <- read.csv('metrics/surprisals_sensitivity_sand.tsv',sep='\t',header=F)

colnames(data.df) <- c(.0001,.001,.01,.1,1)

alpha.sensitivity.sand <- ggpairs(data.df)

save.image(file='notebook_data.RData')


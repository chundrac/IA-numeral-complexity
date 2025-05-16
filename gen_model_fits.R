require(mgcv)
require(marginaleffects)
require(tikzDevice)
require(lme4)
require(raster)
require(ggplot2)

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
options(ggplot2.discrete.fill = cbPalette)
options(ggplot2.discrete.colour = cbPalette)

#

### generate average metrics, UniNum

MDL <- read.csv('metrics/MDL_uninum.tsv',sep='\t')

LDLprod <- read.csv('metrics/LDL_production_uninum.tsv',sep='\t')
LDLprod <- aggregate(PER ~ language, LDLprod, FUN=mean)

LDLcomp <- read.csv('metrics/LDL_comprehension_uninum.tsv',sep='\t')
LDLcomp <- aggregate(accuracy ~ language, LDLcomp, FUN=mean)

surprisal <- read.csv('metrics/surprisals_uninum.tsv',sep='\t')
surprisal <- aggregate(surprisal ~ language, surprisal, FUN=mean)

merged <- merge(MDL, LDLprod, by='language')
merged <- merge(merged, LDLcomp, by='language')
merged <- merge(merged, surprisal, by='language')

metadata <- read.csv(url('https://raw.githubusercontent.com/numeralbank/googleuninum/master/cldf/languages.csv'))

merged <- merge(merged,metadata,by.x='language',by.y='ID')
merged <- na.omit(merged)

merged.PCA.uninum <- prcomp(merged[,c('surprisal','PER','MDL','accuracy')])

merged$PC1 <- merged.PCA.uninum$x[,1]

### merge w. AUTOTYP register

register <- read.csv('https://raw.githubusercontent.com/autotyp/autotyp-data/refs/heads/master/data/csv/Register.csv')

merged <- merge(merged,register,by='Glottocode')

merged$SouthAsia <- ifelse(merged$Area == 'Indic', 'S Asia', 'Rest of World')

model.PC1.uninum = lmer(PC1 ~ SouthAsia + (1|Family), merged)

model.PC1.uninum.predictions <- plot_predictions(model.PC1.uninum, condition='SouthAsia')

tikz('graphics/PC1-predictions-uninum.tex',width=7*.5,height=7*.5)
model.PC1.uninum.predictions
dev.off()

#

MDL <- read.csv('metrics/MDL_sand.tsv',sep='\t')

LDLprod <- read.csv('metrics/LDL_production_sand.tsv',sep='\t')
LDLprod <- aggregate(PER ~ language, LDLprod, FUN=mean)

LDLcomp <- read.csv('metrics/LDL_comprehension_sand.tsv',sep='\t')
LDLcomp <- aggregate(accuracy ~ language, LDLcomp, FUN=mean)

surprisal <- read.csv('metrics/surprisals_sand.tsv',sep='\t')
surprisal <- aggregate(surprisal ~ language, surprisal, FUN=mean)

merged <- merge(MDL, LDLprod, by='language')
merged <- merge(merged, LDLcomp, by='language')
merged <- merge(merged, surprisal, by='language')

metadata <- read.csv(url('https://raw.githubusercontent.com/numeralbank/sand/main/cldf/languages.csv'))

metadata <- rbind(metadata,read.csv('extra_data/languages.csv'))

metadata <- metadata[,c('ID','Glottocode','Latitude','Longitude','Family','Base')]

#fix Brokskad, which is associated with Brokkad (TB, Bhutan), not Shina dialect
levels(metadata$Glottocode) <- c(levels(metadata$Glottocode),'brok1247')
metadata[metadata$ID == 'Brokskad',]$Glottocode <- 'brok1247'
metadata[metadata$ID == 'Brokskad',]$Latitude <- 34.53
metadata[metadata$ID == 'Brokskad',]$Longitude <- 76.60

merged <- merge(merged,metadata,by.x='language',by.y='ID')
merged <- na.omit(merged)

merged.PCA.sand <- prcomp(merged[,c('surprisal','PER','MDL','accuracy')])

merged$PC1 <- merged.PCA.sand$x[,1]

merged$IndoAryan <- ifelse(merged$Family == 'Indo-Aryan', 'Indo-Aryan', 'Other families')

model.PC1.sand = lm(PC1 ~ IndoAryan, merged)

model.PC1.sand.predictions <- plot_predictions(model.PC1.sand, condition='IndoAryan')

tikz('graphics/PC1-predictions-sand.tex',width=7*.5,height=7*.5)
model.PC1.sand.predictions
dev.off()

elevation_world <- getData('worldclim', var='alt', res=2.5)

merged$elevation <- extract(elevation_world,merged[,c('Longitude','Latitude')])

merged$vigesimal <- grepl('Vigesimal',merged$Base)

gam.elev <- gam(PC1 ~ s(elevation), data=merged[merged$Family=='Indo-Aryan',])

gam.vigesimal <- gam(PC1 ~ vigesimal, data=merged[merged$Family=='Indo-Aryan',])

gam.elev.vigesimal <- gam(PC1 ~ vigesimal + s(elevation), data=merged[merged$Family=='Indo-Aryan',])

pred.vigesimal <- plot_predictions(gam.elev.vigesimal,by='vigesimal')

slope.elevation <- plot_slopes(gam.elev.vigesimal,variables='elevation',by='elevation')

#all IA langs in https://raw.githubusercontent.com/numeralbank/googleuninum/b9ece9f6048915cf2846bdfa0d5fd4c338620d69/cldf/languages.csv
IA <- c('asm','awa','ben','bgc','ctg','dcc','dgo','guj','hin','hne','hoj','kas','gom','mag','mai','mar','mup','mwr','npi','ori','pnb_pk','pnb_in','rkt','san','sin','skr_no_diacritics','r_with_diacritics','snd','syl','urd')

# Surprisal

data.df <- read.csv('metrics/surprisals_uninum.tsv',sep='\t',stringsAsFactors = T)

data.df$Group <- as.factor(ifelse(data.df$language %in% IA, 'IA', 'non-IA'))

model.surprisal <- gam(surprisal ~ Group + s(number,by=Group) + s(language,bs='re') + s(number,language,bs='re'), family=gaussian(link=log), data=data.df)

plot.surprisal <- plot_predictions(model.surprisal, by=c("number","Group")) + ylab('Surprisal') + xlab('Cardinality') + theme_bw()

slopes.surprisal <- plot_slopes(model.surprisal,variables='number',by='Group') + theme_bw()

intercepts.surprisal <- plot_predictions(model.surprisal,by='Group') + theme_bw()

tikz('graphics/surprisal-by-cardinality.tex',width=7*.5,height=7*.5)
plot.surprisal
dev.off()

# Comprehension

data.df <- read.csv('metrics/LDL_comprehension_uninum.tsv',sep='\t',stringsAsFactors = T)

data.df$Group <- as.factor(ifelse(data.df$language %in% IA, 'IA', 'non-IA'))

model.comprehension <- gam(accuracy ~ Group + s(number,by=Group) + s(language,bs='re') + s(number,language,bs='re'), family=binomial(link=logit), data=data.df)

plot.comprehension <- plot_predictions(model.comprehension, by=c("number","Group")) + ylab('Comprehension Accuracy') + xlab('Cardinality') + theme_bw()

slopes.comprehension <- plot_slopes(model.comprehension,variables='number',by='Group') + theme_bw()

intercepts.comprehension <- plot_predictions(model.comprehension,by='Group') + theme_bw()

tikz('graphics/comprehension-by-cardinality.tex',width=7*.5,height=7*.5)
plot.comprehension
dev.off()

# Production

data.df <- read.csv('metrics/LDL_production_uninum.tsv',sep='\t',stringsAsFactors = T)

data.df$Group <- as.factor(ifelse(data.df$language %in% IA, 'IA', 'non-IA'))

model.production <- gam(PER ~ Group + s(number,by=Group) + s(language,bs='re') + s(number,language,bs='re'), family=binomial(link=logit), data=data.df)

plot.production <- plot_predictions(model.production, by=c("number","Group")) + ylab('Production Error Rate') + xlab('Cardinality') + theme_bw()

slopes.production <- plot_slopes(model.production,variables='number',by='Group') + theme_bw()

intercepts.production <- plot_predictions(model.production,by='Group') + theme_bw()

tikz('graphics/production-by-cardinality.tex',width=7*.5,height=7*.5)
plot.production
dev.off()

save.image(file='models.Rdata')



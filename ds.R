library(maptools)
library(ggplot2)
library(rgdal)
library(broom)
library(raster)
library(readxl)
library(viridis)
library(reshape)

options(stringsAsFactors = F)

# Carico lo shapefile tramite la funzione readOGR (pacchetto rgdal)
shp <- readOGR("shp")
shp@data$id <- rownames(shp@data)

# Carico il file excel
com <- read_excel('comuni.xlsx')

# Faccio il merge tra lo shapefile e il file excel dei dati
shp <- merge(shp, com)

# Creo una nuova mappa contenente solo i comuni presenti nel file excel
shp2 <- subset(shp, !(is.na(shp@data$DS_PROG)))

# Plotto la nuova mappa
ggplot() +
  geom_polygon(data = shp2, mapping = aes(x=long, y=lat, group=group))



# Converto la nuova mappa in un dataframe e faccio il merge dei dati
ds <- broom::tidy(shp2)
ds <- merge(ds, shp2)

cnames <- recast(ds, DS_DISTRETTO+CODS_DIS~variable, measure.var = c('long', 'lat'), mean)
t1 <- cnames[, c('CODS_DIS', 'DS_DISTRETTO')]
t1$int <- floor(t1$CODS_DIS/10)



g1 <- ggplot() +
  theme_void() +
  geom_polygon(data = ds, mapping = aes(x=long, y=lat, group=group, fill=DS_DISTRETTO), color=NA) +
  #geom_polygon(data = ds, mapping = aes(x=long, y=lat, group=group), color='black', fill=NA, size=0.001) +
  geom_text(data = cnames, aes(x = long, y = lat, label=CODS_DIS), size=1, color='white') +
  coord_equal() +
  scale_fill_viridis(discrete = T) +
  theme(legend.position = 'none')
  

  library(gridExtra)
  library(grid)
  library(lattice)

mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.5)),
    colhead = list(fg_params=list(cex = 0.1)),
    rowhead = list(fg_params=list(cex = 0.1)))



pdf('prova4.pdf')
  lay <- rbind(c(1,2),
               c(1,2))
  grid.arrange(g1, tableGrob(t1, theme = mytheme), layout_matrix = lay)
dev.off()


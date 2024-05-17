## ----install packages-------------------------------------------------------------------------------------------------------------------------
#| eval: false

## install.packages(c('readxl',  'sf', 'terra', 'leaflet', 'geodata',  'wbstats',
##                    'rnaturalearth', 'osmextract', 'osrm', 'plotly'))


## ----from_scratch-----------------------------------------------------------------------------------------------------------------------------
# 3 vecteurs avec nom, longitude et latitude
name <- c("Sentido Bellevue Park", "Université de Sfax", "Université Paris Cité")
long <- c(10.579, 10.742, 2.382)
lat <- c(35.913, 34.736, 48.827)

# Transformer en data.frame
loc <- data.frame(name, long, lat)

# Transformer en objet spatial
library(sf)
loc <- st_as_sf(loc, 
                coords = c("long", "lat"),
                crs = 4326)

# Visualiser simplement avec leaflet
library(leaflet)

leaflet(loc) |> 
  addTiles() |> 
  addCircleMarkers(popup = loc$name) # Hôpitaux



## ----import_csv-------------------------------------------------------------------------------------------------------------------------------
del_df <- read.csv("data/tun/data/don_del.csv", sep = ";", dec = ",")


## ----R_objects--------------------------------------------------------------------------------------------------------------------------------
class(del_df)
str(del_df)


## ----head_object------------------------------------------------------------------------------------------------------------------------------
head(del_df, 5)


## ----import_excel-----------------------------------------------------------------------------------------------------------------------------
library(readxl)

world <- read_excel("data/world/data/unpp_POP_5y.xlsx", 
                 sheet = "Estimates",
                 skip = 16,
                 col_types = c(rep("text", 10), rep("numeric", 22))) 


## ----data_frame_classes-----------------------------------------------------------------------------------------------------------------------
# Conversion en data.frame
world <- data.frame(world)

# Nom des colonnes
colnames(world)

# Années disponibles
unique(world$Year)


## ----data_frame_handling----------------------------------------------------------------------------------------------------------------------
# Somme de plusieurs colonnes
world$POP_TOT <- rowSums(world[,c(12:32)]) # Population totale

# Renommer une colonne
names(world)[3] <- "Name"

# Créer des indicateurs
# Part des jeunes
world$YOUNG_RT <- (world$X0.4 + world$X5.9 + world$X10.14) / world$POP_TOT * 100 
# Part des personnes âgées
world$OLD_RT <- (world$X65.69 + world$X70.74 + world$X75.79 + world$X80.84 +
                   world$X85.89 + world$X90.94 + world$X95.99) / world$POP_TOT * 100 

# Sélection d'une ligne
world_2021 <- world[world$Year == 2021,] # Année de référence

# Sélectionner plusieurs lignes
afr <- c("910", "911", "912", "913", "914")
afr_2021 <- world_2021[world_2021$Parent.code %in% afr,] # Pays africains

# Ordonner selon les valeurs d'une colonne
afr_2021 <- afr_2021[order(afr_2021$YOUNG_RT, decreasing = TRUE),]


## ----summary----------------------------------------------------------------------------------------------------------------------------------
# Résumé stat
summary(afr_2021)


## ----tunisian_data_handling-------------------------------------------------------------------------------------------------------------------
# Import
input_gouv <- read_excel("data/tun/data/chomage.xls", # Chemin du fichier
                         sheet = "Sheet1", # Nom de la feuille Excel
                         skip = 3, # Retirer les trois premières lignes
                         col_types = c(rep("text", 3), rep("numeric", 6))) # Format des colonnes 

# Table de passage Recensement / codes internationaux
gouv <- read.csv("data/tun/data/code_tun.csv")

# Reformatage
input_gouv <- data.frame(input_gouv)
input_gouv[,1:2] <- NULL # Retirer les deux premières colonnes

# Noms de colonnes
cols <- c("CHOM_INS_NON_", "CHOM_INS_PRI_", "CHOM_INS_SEC_", "CHOM_INS_SUP_",
          "CHOM_INS_UNK_", "CHOM_INS_TOT_") 
colnames(input_gouv)[1] <- "id_TUN"

# Chômeurs
# Hommes
tmp1 <- input_gouv[c(1:25),]
names(tmp1)[2:length(tmp1)] <- paste0(cols, "M")
gouv <- merge(gouv, tmp1, by = "id_TUN", all.x = TRUE)

# Femmes
tmp2 <- input_gouv[c(26:50),]
names(tmp2)[2:length(tmp2)] <- paste0(cols, "F")
gouv <- merge(gouv, tmp2, by = "id_TUN", all.x = TRUE)


## ----tunisian_boxplot-------------------------------------------------------------------------------------------------------------------------
#| fig-cap: "Nombre de chômeurs dans les gouvernorats tunisiens par niveau d'éducation"
# 2 graphiques par ligne
par(mfrow = c(1,2), mar = c(2,2,2,2))

# Boxplot hommes
boxplot(gouv$CHOM_INS_NON_M, # Chômeurs hommes par niveau d'instruction
        gouv$CHOM_INS_PRI_M, 
        gouv$CHOM_INS_SEC_M,
        gouv$CHOM_INS_SUP_M, 
        ylim = c(0, 12000), # Bornes min/max de l'axe des ordonnées
        main = "Hommes", # Titre plot
        names = c("Rien", "Primaire", "Secondaire", "Tertiaire"), # Labels (X)
        ylab = "Nombre de chômeurs, 2014", # Label (Y)
        col = "#adcaf7", # Couleur des box-plots
        cex.axis = .6, # Taille des labels des axes (réduit de 70 %)
        cex.title = .6) # Taille du label du titre (réduit de 70 %)

# Boxplot femmes
boxplot(gouv$CHOM_INS_NON_F, 
        gouv$CHOM_INS_PRI_F, 
        gouv$CHOM_INS_SEC_F,
        gouv$CHOM_INS_SUP_F,
        ylim = c(0, 12000),
        main = "Femmes",
        names = c("Rien", "Primaire", "Secondaire", "Tertiaire"), 
        col = "#ed9fb0", 
        cex.axis = .6, 
        cex.title = .6)


## ----write_csv--------------------------------------------------------------------------------------------------------------------------------
#| eval: false
## write.csv(x = gouv, # Objet à exporter
##           file = "data/tun/data/gouv_chom.csv", # Chemin de fichier
##           row.names = FALSE) # Pour retirer les numéros de ligne


## ----plot_r_base------------------------------------------------------------------------------------------------------------------------------
par(mar = c(2,2,2,2), mfrow = c(2, 3))

barplot(afr_2021$YOUNG_RT, main = "Diagramme en barre")
boxplot(afr_2021$YOUNG_RT, main = "Boîtes à moustache")
hist(afr_2021$YOUNG_RT, main = "Histogrammes")
hist(afr_2021$YOUNG_RT, freq = FALSE, main = "Histogrammes et densité")
lines(density(afr_2021$YOUNG_RT), col = "blue")
stripchart(afr_2021$YOUNG_RT, method = "jitter", pch = 16,
           main = "Diagrammes de dispersion")
plot(data = afr_2021, YOUNG_RT ~ OLD_RT, main = "Nuages de points")


## ----plot_r_base_params-----------------------------------------------------------------------------------------------------------------------
par(mar = c(4,4,0,4))

# Line plot
tun <- world[world$Name == "Tunisia",]
alg <- world[world$Name == "Algeria",]
mar <- world[world$Name == "Morocco",]

plot(alg$Year, # Abscisses 
     alg$YOUNG_RT, # Ordonnées
     type = "l", # Type lignes
     ylim = c(20, 50), # Bornes min/max des ordonnées
     cex = .6, # Taille des points
     col = "blue", # Couleur de la ligne
     cex.lab = 0.7, # Réduit les labels d'un facteur de 0.7
     cex.axis = 0.6, # Réduit les labels des graduations d'un facteur de 0.6 
     xlab = "Années", # Label abscisses 
     ylab = "Part des jeunes (%) dans la population totale") # Label ordonnées

lines(mar$Year, 
      mar$YOUNG_RT, # Rajouter une ligne (Maroc)
      col = "darkgreen", 
      type = "l",
      cex = .6)

lines(tun$Year,  # Et Tunisie
      tun$YOUNG_RT, 
      type = "l", 
      cex = .6,
      col = "red")

# Organisation de la légende
legend(x = "topright",
       legend = c("Algérie", "Maroc", "Tunisie"),
       col = c("blue", "darkgreen", "red"), lty = 1,
       cex = .8)


## ----plotly-----------------------------------------------------------------------------------------------------------------------------------
library(plotly)

# Importer jeu de données d'exemple de gapminder
gap <- read.csv("data/world/data/gapminder.csv")

head(gap)

# Paramétrage des cercles proportionnels
area_max <- 2000 # Diamètre maximal
area_min <- area_max/(max(gap$pop)/min(gap$pop)) # Diamètre minimal

# Créer un graphique de type "Multiple Trace Animations"
fig <- gap %>%
  plot_ly(
    x = ~gdpPercap, # Variable X
    y = ~lifeExp,   # Variable Y
    size = ~pop,    # Taille des cercles 
    color = ~continent,  # Couleur des cercles 
    frame = ~year, # Animation temporelle 
    sizes = c(area_min, area_max), # Gestion taille cercles
    marker = list(opacity = 0.5, sizemode = 'area'), # Taille et opacité
    text = ~country,  #  Nom du champ qui s'affichera interactivement
    hoverinfo = "text", 
    type = 'scatter', # Type de graphique
    mode = 'markers' # Mode de représentation des figurés
  )

# Paramétrer l'échelle, les labels, etc.
fig <- fig %>% layout(
    xaxis = list(type = "log", title = "PIB par habitant"),
    yaxis = list(title = "Espérance de vie")
  )

fig


## ----st_layers--------------------------------------------------------------------------------------------------------------------------------
library(sf)

st_layers("data/tun/geom/tun_admin.gpkg")


## ----import_sf--------------------------------------------------------------------------------------------------------------------------------
del <- st_read("data/tun/geom/tun_admin.gpkg", layer = "delegation", quiet = TRUE)
gouv <- st_read("data/tun/geom/tun_admin.gpkg", layer = "gouvernorat", quiet = TRUE)
reg <- st_read("data/tun/geom/tun_admin.gpkg", layer = "region", quiet = TRUE)


## ----class_sf---------------------------------------------------------------------------------------------------------------------------------
class(del)


## ----import_geojson---------------------------------------------------------------------------------------------------------------------------
reg <- st_read("data/tun/geom/map_reg.geojson", quiet = TRUE) 


## ----head_sf----------------------------------------------------------------------------------------------------------------------------------
head(del)


## ----plot_sf----------------------------------------------------------------------------------------------------------------------------------
plot(del)


## ----plot_geom--------------------------------------------------------------------------------------------------------------------------------
#| fig-height: 4

# 3 cartes par ligne
par(mfrow = c(1,3), mar = c(2,2,2,2))

# Délégations
plot(del$geom, # Géométries uniquement
     col = "peachpuff", # Couleur du fond
     border = "white", # Couleur de bordure
     main = "Délégations") # Titre

# Gouvernorats
plot(gouv$geom, 
     col = "peachpuff", 
     border = "white",
     main = "Gouvernorats")

# "Régions"
plot(reg$geom, 
     col = "peachpuff",
     border = "white",
     main = "Régions")


## ----merge------------------------------------------------------------------------------------------------------------------------------------
del <-  merge(
  x = del[,"del_code"],  # L'objet sf (seulement le champ del_code)
  y = del_df,          # le data.frame
  by.x = "del_code",  # identifiant dans x
  by.y = "del_code",  # identifiant dans y
  all.x = TRUE         # conserver toutes les lignes
)

head(del)
plot(del[,"idr_2011"])


## ----sf_select--------------------------------------------------------------------------------------------------------------------------------
# Sélection de lignes
sou <- del[del$gou_nom == "Sousse",]

# Sélection de colonnes
sou <- sou[,"idr_2011"]

# Ne conserver que les lignes avec une valeur
sou <- sou[!is.na(sou$idr_2011),]

plot(sou[,"idr_2011"])


## ----sf_export--------------------------------------------------------------------------------------------------------------------------------
#| eval: false
## st_write("data/tun/geom/sousse_deleg.geojson")


## ----import_terra-----------------------------------------------------------------------------------------------------------------------------
library(terra)
pop <- rast("data/tun/geom/pop.tif")


## ----class_terra------------------------------------------------------------------------------------------------------------------------------
pop


## ----summary_terra----------------------------------------------------------------------------------------------------------------------------
summary(pop)


## ----plot_terra-------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1,2))

# Graphique en échelle continue
plot(pop)

# Avec discrétisation et paramétrage des couleurs
cuts <- c(0, 1, 2, 4, 8, 16, 32, 64, 235)
cols <- colorRampPalette(c("yellow", "darkgoldenrod1", "brown1"))
plot(pop, breaks = cuts, col = cols(8))


## ----crop_terra-------------------------------------------------------------------------------------------------------------------------------
# Transformer en WGS 84
sou <- st_transform(sou, 4326)

# Crop avec la délégation de Sousse
pop_sou <- crop(pop, sou)

# Plot
plot(pop_sou, breaks = cuts, col = cols(8))
plot(sou$geometry, col = NA, add = TRUE)


## ----export_terra-----------------------------------------------------------------------------------------------------------------------------
#| eval: false
## writeRaster(x = pop_sou, filename = "data/tun/geom/pop_sou.tif")


## ----import_geodata---------------------------------------------------------------------------------------------------------------------------
#| eval: false

## library(geodata)
## elev <- elevation_30s(country = "TUN", path = tempdir())
## temp <- worldclim_country(country = "Tunisia",
##                           res = 10,
##                           var = "tavg",
##                           path = tempdir())


## ----import_geodata_hide----------------------------------------------------------------------------------------------------------------------
#| echo: false

elev <- rast("data/tun/geom/elevation.tif")
temp <- rast("data/tun/geom/temp.tif")


## ----class_geodata----------------------------------------------------------------------------------------------------------------------------
class(temp)
class(elev)


## ----plot_geodata-----------------------------------------------------------------------------------------------------------------------------
# Les afficher
par(mfrow = c(1,2))

# Altitude
cols <- colorRampPalette(c("#31ad37", "#f5f752", "#fca330", "#9c5903"))
plot(elev, main = "Altitude", col = cols(50))
     
# Température
cols <- colorRampPalette(c("#4575b4", "#91bfdb", "#e0f3f8", 
                           "#fee090", "#fc8d59", "#d73027"))
plot(temp$TUN_wc2.1_30s_tavg_5, main = "Températures, Mai (1970-2000)",
     col = cols(50))


## ----import_naturalearth----------------------------------------------------------------------------------------------------------------------
library(rnaturalearth)

# Import des pays
country <- ne_countries(type = "countries", # pays
                        scale = "small",  # niveau de généralisation
                        returnclass = "sf") # objet retourné

# Conversion en projection Mercator
country <- st_transform(country, crs = "EPSG:3857")

# Si on s'intéresse à l'Afrique (modèle carto)
afr <- country[country$continent == "Africa",]


## ----search_wbstats---------------------------------------------------------------------------------------------------------------------------
library(wbstats)

wb_search("CO2 emissions")


## ----import_wbstats---------------------------------------------------------------------------------------------------------------------------
# Sélection des indicateurs
my_indicators = c("pop" = "SP.POP.TOTL",
                  "co2" = "EN.ATM.CO2E.KT")

# Interroger l'API
wb <- wb_data(my_indicators, return_wide = FALSE)

head(wb)
# Années disponibles
unique(wb$date)


## ----select_wbstats---------------------------------------------------------------------------------------------------------------------------
# Sélectionner la dernière année disponible et les colonnes utiles
wb <- wb[wb$date %in% 2020,]
wb <- wb[,c("iso3c", "indicator_id", "value")]
wb <- data.frame(wb)

# Formatage (format long + concaténation code indicateur + année ref
wb <- reshape(wb, idvar = "iso3c", timevar = "indicator_id", direction = "wide")

# Renommage des colonnes et création de l'indicateurs émissions par hab
names(wb)[2:3] <- c("pop_2020", "co2_2020")
wb$co2_hab_2020 <- wb$co2_2020 * 1000 / wb$pop_2020


## ----plot_wbstats-----------------------------------------------------------------------------------------------------------------------------
# Jointure attributaire
afr <- merge(afr[,c("adm0_a3", "name_fr")], wb, 
             by.x = "adm0_a3", by.y = "iso3c", all.x = TRUE) 

# Discretisation
discr <- quantile(afr[,"co2_hab_2020", drop = TRUE], probs = seq(0,1, 0.25), na.rm = TRUE)

# Couleurs
cols <- colorRampPalette(c("yellow", "darkgoldenrod1", "brown1"))

# Carte
plot(afr[,"co2_hab_2020"],
     breaks = "quantile",
     main = "Émission de CO² par habitant, 2020")


## ----import_osmextract------------------------------------------------------------------------------------------------------------------------
#| eval: false
## library(osmextract)
## osm_poly <- oe_get(place = "Tunisia",
##                    layer = "multipolygons",
##                    extra_tags = c("amenity", "ref:tn:hasc_2", "ref:tn:codegeo",
##                                   "name:fr", "name:ar"))
## 
## osm_pt <- oe_get(place = "Tunisia",
##                  layer = "points",
##                  extra_tags = c("amenity", "name:fr", "name:ar"))


## ----prepare_osmextract-----------------------------------------------------------------------------------------------------------------------
#| eval: false
## # Délégations et secteurs
## admin <- osm_poly[!is.na(osm_poly$admin_level),] # Retirer pas d'attribut de niveau hiérarchique
## deleg <- admin[admin$admin_level == 5,] # Délégations
## sect <- admin[admin$admin_level == 6,] # Secteurs
## 
## # Ne garder que les champs utiles
## deleg <- deleg[,c("osm_id", "ref_tn_codegeo", "ref_tn_hasc_2", "name_ar",
##                   "name_fr", "admin_level")]
## sect <- sect[,c("osm_id", "ref_tn_codegeo", "name_ar",
##                   "name_fr", "admin_level")]


## ----consolidate_osmextract-------------------------------------------------------------------------------------------------------------------
#| eval: false

## # Délégation d'appartenance du secteur
## sect_pt <- st_centroid(sect) # Centroide du secteur
## sect_pt <- st_intersection(sect_pt, deleg) # Intersection avec couche délégation
## sect_pt <- st_set_geometry(sect_pt, NULL) # Retirer géométries
## sect <- merge(sect, # Enrichir les secteurs du code d'appartenance de la délégation
##               sect_pt[,c("osm_id", "ref_tn_hasc_2")],
##               by = "osm_id",
##               all.x = TRUE)
## sect <- sect[!duplicated(sect$osm_id),] # Retirer valeurs dupliquées
## 
## # Renommer colonnes
## names(sect)[2] <- "id_tn"
## names(sect)[6] <- "id_hasc_deleg"
## sect$id_hasc_gouv <- substr(sect$id_hasc_deleg, 1, 5) # Gouvernorat d'appartenance
## names(sect)[2,6] <- (c("tn_codegeo", "int_codegeo"))
## deleg$id_hasc_gouv <- substr(deleg$id_hasc_deleg, 1, 5)


## ----export_preparation_osmextract------------------------------------------------------------------------------------------------------------
#| eval: false

## # Consolidation des géométries
## sel_poly <- osm_poly[!is.na(osm_poly$amenity),] # Retirer les objets qui n'ont pas de clé amenity
## sel_pt <- osm_pt[!is.na(osm_pt$amenity),]
## sel_poly <- st_make_valid(sel_poly) # Consolider les géométries des polygones
## sel_pt2 <- st_centroid(sel_poly) # extraire le centroide
## cols <- intersect(names(sel_pt), names(sel_pt2)) # Garder les colonnes identiques
## sel_pt <- rbind(sel_pt[,cols, sel_pt2[,cols]]) # Combiner points et polygones
## sel_pt <- sel_pt[,c("osm_id", "amenity", "name_ar", "name_fr")]
## 
## # Exporter les couche ainsi créées
## st_write(sel_pt, "data/tun/geom/tun_osm.gpkg", layer = "poi")
## st_write(deleg, "data/tun/geom/tun_osm.gpkg", layer = "deleg")
## st_write(sect, "data/tun/geom/tun_osm.gpkg", layer = "sect")


## ----import_conso_osmextract------------------------------------------------------------------------------------------------------------------
# Import des aménités préparées en amont
osm_pt <- st_read("data/tun/geom/tun_osm.gpkg", layer = "poi", quiet = TRUE)

# Nombre de points avec le tag "amenity". 
tbl <- table(osm_pt$amenity)
tbl <- tbl[order(tbl, decreasing = TRUE)]
tbl[1:15]

# On ne prend ici que les cliniques et hôpitaux
osm_pt <- osm_pt[osm_pt$amenity %in% c("clinic", "hospital"), ]


## ----import_admin_osmextract------------------------------------------------------------------------------------------------------------------
# Import des unités territoriales préparées en amont
deleg <- st_read("data/tun/geom/tun_osm.gpkg", layer = "deleg", quiet = TRUE)
sect <- st_read("data/tun/geom/tun_osm.gpkg", layer = "sect", quiet = TRUE)

# Nombre d'objets (lignes)
nrow(deleg)
nrow(sect)
nrow(osm_pt)


## ----leaflet_osmextract-----------------------------------------------------------------------------------------------------------------------
#| code-fold: true

# Choisir la palette
library(leaflet)
pal <- colorFactor(palette = c("red", "gold"),
                   domain = c("clinic", "hospital"))

# Cartographie interactive
leaflet(osm_pt) |> # Emprise = hôpitaux
  addProviderTiles("OpenStreetMap.HOT") |> # Type de tuiles chargées
  addPolygons(data = sect, # Secteurs
              col = "white",
              fillColor = "lightgrey",
              fillOpacity = 0.7,
              weight = 1,
              popup = paste0("<b>", sect$id_tn, "<br></b>",
                             sect$name_ar, "<br>", sect$name_fr),
              group = "Secteurs")|>
  addPolygons(data = deleg, # Délégations
              col = "darkgrey",
              fill = "lightgrey",
              fillOpacity = 0,
              weight = 1.2,
              popup = paste0("<b>", deleg$id_tn, " / ",
                             deleg$id_hasc_deleg,"<br></b>",
                             deleg$name_ar, "<br>", deleg$name_fr),
              group = "Délégations") |>
  addCircleMarkers(radius = 4, # Hôpitaux
                   stroke = FALSE,
                   color = ~ pal(amenity),
                   fillOpacity = 1,
                   popup = paste0(osm_pt$name_ar, "<br>", osm_pt$name_fr),
                   group = "Cliniques et hôpitaux") |>
  addLegend(pal = pal, # Légende pour différencier cliniques et hôpitaux
            values = c("clinic", "hospital"),
            opacity = 0.7,
            title = "OSM amenity",
            position = "bottomright") |>
  addLayersControl(overlayGroups = c("Secteurs", "Délégations", "Cliniques et hôpitaux"),
                   options = layersControlOptions(collapsed = FALSE)) 


## ----prepare_osrm-----------------------------------------------------------------------------------------------------------------------------
# Origines
sel <- sect[sect$id_hasc_gouv == "TN.SS",] # Secteurs de la délégation de Sousse
sel <- sel[!is.na(sel$osm_id),] # Géométries vides
sel <- sel[!duplicated(sel$osm_id),] # Géométries dupliquées
ori <- st_centroid(sel) # Cenroide

# Considérer les cliniques et hôpitaux dans un voisinage de 20 km autour de la délégation
osm_pt <- st_transform(osm_pt, 2088) # Transformer en coordonnées planaires
sel <- st_transform(sel, 2088)
dest <- st_filter(osm_pt, st_buffer(sel, 20000), .predicate = st_intersects) # Dans un rayon de 20 km
dest <- st_transform(dest, 4326)

# Calcul de temps de trajets avec OSRM (pas grosse requête)
library(osrm)
df <- osrmTable(src = ori, dst = dest, measure = "duration")

# Extraire les temps de trajet
df <- data.frame(df$durations)

# Formater la table d'une manière arrangeante
colnames(df) <- as.character(dest$osm_id)
row.names(df) <- as.character(ori$osm_id)
head(df)


## ----time_min_osrm----------------------------------------------------------------------------------------------------------------------------
time <- apply(df, 1, min) 
time <- data.frame(time)
time$osm_id <- row.names(time)
head(time)


## ----map_osrm---------------------------------------------------------------------------------------------------------------------------------
#| code-fold: true
sel <- merge(sel, time, by = "osm_id", all.x = TRUE)
deleg <- st_transform(deleg, 2088)
dest <- st_transform(dest, 2088)

library(mapsf)
par(mfrow = c(1,1))
mf_init(sel)
mf_map(deleg, col = "lightgrey", 
       border = NA,
       add = TRUE)
mf_map(sel, 
       type = "choro",
       var = "time",
       nbreaks = 4,
       border = "white", 
       leg_title = "Minutes en voiture", 
       add = TRUE)
mf_map(deleg, col = NA, 
       border = "black",
       add = TRUE)
mf_map(dest, pch = 21, col = NA, bg = "red", add = TRUE)
mf_scale(size = 10)
mf_title("Délégation de Sousse : Temps de trajet vers l'hôpital ou la clinique la plus proche")
mf_credits(paste0("Source: © OpenStreetMap et Contributeurs, 2024\n",
                  "NB/ Toute chose égale par rapport à la complétude d'OSM. Contribuez pour compléter la carte le cas échéant !"))


## ---------------------------------------------------------------------------------------------------------------------------------------------
#  Import des données
del <- read.csv("data/tun/data/don_del.csv", sep = ";")
head(del)

# Évolution (indice 100) de l'équipement de la population en ordinateur
del$ordin_evol <- del$ordin_2014 / del$ordin_2004 * 100
head(del[,c("del_code", "del_nom_fr", "ordin_2004", "ordin_2014", "ordin_evol")])

# Résumé statistique
summary(del$ordin_evol)

# Histogramme
hist(del$ordin_evol)

# Import géométries
library(sf)
del_geom <- st_read("data/tun/geom/tun_admin.gpkg", layer = "delegation")

# Jointure attributaire
del_geom <- merge(del_geom, del, by = "del_code")

# Carte simple
plot(del_geom[,"ordin_evol"], breaks = "quantile",
     main = "Une géographie assez marquée...")


## ---------------------------------------------------------------------------------------------------------------------------------------------
sessionInfo()


###########################################################################################################################
#######                               Extraction des Données d'Altitude en Suisse                                   #######
###########################################################################################################################


############################################# téléchargement des packages #################################################

# installation si nécessaire:
# install.packages(c("raster", "rnaturalearth", "ggplot2", "elevatr"))

library(rnaturalearth)                                            # cartographie des pays
library(ggplot2)                                                  # faire les graphique
library(raster)                                                   # analyse spatiale (lire et manipuler raster)
library(elevatr)                                                  # téléchargement des données d'altitude

# faire comprendre à R de faire les calculs surface plane 
sf_use_s2(FALSE)                                                  # évite message d'erreurs sur mac



############################################  téléchargement carte de suisse   ############################################

# téléchargement de la carte suisse via Natural Earth
Switzerland <- ne_countries(                                      # chargement des données cartographiques
    scale = "medium",                                             # niveau détail des frontières
    returnclass = "sf",                                           # tableau coordonnées & manipulation
    country = "switzerland"                                       # pays ciblé
)   



########################################  téléchargement des données d'altitude   #########################################

# téléchargment image satellite d'altitude qui couvre la suisse
elevation_switzerland <- get_elev_raster(Switzerland, z = 8)      # z est le niveau de zoom (résolution)      

# visualisation du raster d'altitude
plot(elevation_switzerland)



##############################  convertir les coordonnées des espèces en points spatiaux   ################################

# vérification de la composition de la matrice des espèces
head(full_data)                                                   # species/latitude/longitude/date/source/année

# convertir les coordonées en points spatiaux
spatial_points <- SpatialPoints(
  coords = full_data[, c("longitude", "latitude")],               # extraction des colonnes d'intérêt du tableau
  proj4string = CRS("+proj=longlat +datum=WGS84")                 # code standard pour coordonnées GPS classiques
)

# visualisation des occurences sur la carte de Suisse
plot(elevation_switzerland,                                       # données du raster limitées à la Suisse
      main = "Species Occurrences on Ecosystem Map"               # titre
)                                                                 # affiche raster pour y ajouter les occurences
plot(spatial_points,                                              # données GPS         
      add = TRUE,                                                 # garde carte raster en dessous      
      pch = 16,                                                   # style rond plein
      cex = 1.2,                                                  # taille des points d'occurence
      col = "#b50072"                                           # couleur des points d'occurence
)                             



###############################  garder uniquement les données d'altutide des occurences   ################################

# où est la coccinelle x quelle altitude à cette position
elevation <- raster::extract(
  elevation_switzerland,                                          # raster d'altitude 
  spatial_points,                                                 # cibles (points des coccinelles)
)                                                                 # garde ordre des valeur (point GPS/ordre d'altitude)

# check point de correspondance
identical(crs(elevation_switzerland), crs(spatial_points))        # TRUE (match)

# check point des NA
sum(is.na(full_data_elev$elevation))                              # tous les points ont pu recevoir d'altitude

# ajout de ces données d'élévation au tableau initial
full_data_elev <- data.frame(                                     # fusion des deux tableaux
  full_data,                                                      # tableau de base
  elevation = elevation                                           # données d'altitude
)

# vérification globale du tableau après ajout des données d'altitude
summary(full_data_elev)                                           # altitude données numériques correctes (min, max, ...)

# visualisation du tableau
View(full_data_elev)                                              # tout semble correct (tableau de base + altitude)



#####################################################  check point  #######################################################

# répartition des espèces selon altitude
x11()
ggplot(full_data_elev,                                            # nouveau tableau avec colonne altitude
      aes(x = species,y = elevation,fill = species)) +            # définition des axes du graphique
  geom_boxplot() +                                                # boxplot
  labs(title = "Distribution de l'altitude par espèce",           # titre principal
       y = "Altitude (mètres)", x = "Espèce") +                   # titre axe
  scale_fill_manual(                                              # choix des couleurs pour les points
    values = c("Harmonia axyridis" = "#f08800",                 # orange pour Harmonia axyridis (invasive)
    "Coccinella septempunctata" = "#b50072"),                   # rose pour Coccinella septempunctata (native)
    name = "Espèces",                                             # titre de la légende des couleurs
  ) +
  theme_minimal()                                                 # style de graphique épuré

# analyse du check point 
# données d'environ de 200m (basse altitude suisse) à 3000m (montagne) cohérent avec la topographie de la suisse
# point outliers au dessus de 2000m moins nombreux car conditions rudes
# plus de coccinelle native outliers logique car espèce généraliste comparée à l'invasive qui est spécialiste
# d'où ùédiane plus basse pour Harmonia axyridis et étendue plus large pour Coccinella septempunctata
# cf. graphique effectuer durant l'analyse et interprétation des données

# vérification d'une réelle différence par curiosité
wilcox.test(elevation ~ species, data = full_data_elev)           # distribution non-normale

# analyse du test
# W = 19501862, p-value < 2.2e-16
# différence très significative 
# les espèces n'ont bien pas la même niche altitudinale 









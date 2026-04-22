###########################################################################################################################
#######                             Ajout des Données écosystémique au Jeu de Données                               #######
###########################################################################################################################


############################################# téléchargement des packages #################################################

# installation si nécessaire:
# install.packages(c("raster", "rnaturalearth", "ggplot2", "sf"))

library(rnaturalearth)                                            # accéder aux fonds de cartes 
library(ggplot2)                                                  # création de graphiques et cartes
library(raster)                                                   # manipuler données géographiques en grille 
library(sf)                                                       # manipuler données vectorielles 



######################################### téléchargement du raster ecosystem ##############################################

# chemin d'accès vers le fichier de données écosystémiques 
file_path <- "./data/WorldEcosystem.tif"                          # format GeoTIFF                        

# chargement du fichier raster dans R
ecosystem_raster <- raster(file_path)                             # données codées numériquement                    

# affichage des propriétés du raster 
print(ecosystem_raster)                                           # résolution, étendue, système de coordonnées, ...

# affichage simple de la carte mondiale des écosystèmes 
plot(ecosystem_raster, main = "Original Ecosystem Raster")        # carte du monde avec échelle numérique colorées    



###########################################  limiter les données à la suisse   ############################################

# récupération des frontières de la Suisse
Switzerland <- ne_countries(                                      
    scale = "medium",                                             # précision de la carte moyenne
    returnclass = "sf",                                           # format de sortie 'Simple Features' 
    country = "switzerland"                                       # ciblage du pays (Suisse)
)   

# visualisation des frontières suisses chargées 
plot(st_geometry(Switzerland), main = "Frontières Suisses")       # extrait polygone uniquement du 'sf'   

# découpage du raster en rectangle autour de la suisse
r2 <- crop(ecosystem_raster, extent(Switzerland))                 # permet d'alléger les données

# masquage des données inutiles 
ecosystem_switzerland <- mask(r2, Switzerland)                    # pixels situés uniquement dans les frontières suisses

# affichage du résultat final pour la zone d'étude 
plot(ecosystem_switzerland,                                       # raster limité à la suisse
  main = "Ecosystem Raster Restricted to Switzerland"             # titre        
)                                                                 



##############################  convertir les coordonnées des espèces en points spatiaux   ################################

# affichage des premières lignes pour vérifier les colonnes
head(full_data)                                                   # longitude & latitude présents                                               

# transformation des données en objet spatial 
spatial_points <- SpatialPoints(
  coords = full_data[, c("longitude", "latitude")],               # sélection des colonnes de coordonnées
  proj4string = CRS("+proj=longlat +datum=WGS84")                 # système de coordonnées (GPS classique)
)

# superposition des points sur la carte raster
plot(ecosystem_switzerland,                                       # données du raster limitées à la Suisse
      main = "Species Occurrences on Ecosystem Map"               # titre
)                                                                 # affiche raster pour y ajouter les occurences
plot(spatial_points,                                              # données GPS         
      add = TRUE,                                                 # garde carte raster en dessous      
      pch = 16,                                                   # style rond plein
      cex = 1.2,                                                  # taille des points d'occurence
      col = "#b50072"                                           # couleur des points d'occurence
)                                                                 



#############################  garder uniquement les données écosystémiques des occurences   ##############################

# valeur du pixel pour chaque point d'observation
eco_values <- raster::extract(                                    # utilise spécifiquement 'extract' de raster
  ecosystem_switzerland, spatial_points                           # données de raster, données GPS des occurences
)

# vérification des premières valeurs extraites 
head(eco_values)                                                  # code numérique                              

# ajout des données au tableau de base 
full_data_eco <- data.frame(full_data, eco_values)                # fusion de la colonne extraite avec le tableau de base            

# vérification que la nouvelle colonne est bien présente
head(full_data_eco)                                               # colonne ajoutée avec succès 



########################  ajout métadonnées écosystémiques des occurences au tableau de base   ############################

# chargement fichier permettant de donner correspondance numéro/écosystème
metadata_eco <- read.delim("./data/WorldEcosystem.metadata.tsv")  

# vérification du fichier de métadonnées
head(metadata_eco)                                                # code, température, moisture, landcover, ...

# fusion finale
full_data_meta_eco <- merge(                                      # combine tableaux données de base + éco et métadonnées
  full_data_eco,                                                  # via colonne commune 
  metadata_eco,
  by.x = "eco_values",                                            # nom colonne commune dans tableau (eco_value)
  by.y = "Value"                                                  # nom colonne correspondante dans métadonnées (value)
)
                              
# inspection du tableau final avec les vrais noms de climats
View(full_data_meta_eco)                                          # tout à l'air correct



#####################  visualisation du nombre d'observation par catégorie de climat et espèce   ##########################

# affichage graphique en barres
x11()
ggplot(full_data_meta_eco,aes(x = Climate_Re, fill = species))+   # axe
  geom_bar(position = "fill") +                                  # barre côte à côte et non empilées
  labs(
    title = "Nombre d'observations par espèce selon le climat",   # titre principal
    x = "Catégorie climatique",                                   # type d'écosystème
    y = "Nombre d'observations"                                   # occurences
  ) +
  scale_fill_manual(                                              # choix des couleurs pour les points
    values = c("Harmonia axyridis" = "#f08800",                 # orange pour Harmonia axyridis (invasive)
    "Coccinella septempunctata" = "#b50072"),                   # rose pour Coccinella septempunctata (native)
    name = "Espèces",                                             # titre de la légende des couleurs
  ) +
  theme_minimal()                                                 # style de graphique épuré

# analyse du check point
# prédominance 'Cool Temperate Moist' cohérent avec la suisse soit le plateau et moyenne altitude
# grande densité humaine dans ces zones d'où nombre observation donc pic justifié et cohérent
# plus forte densité d'Harmonia axyridis dans les zones 'Warm Temperate' et 'Cool Temperate' 
# cohérent avec l'invasion et la dominance de la coccinelle invasive dans niche partagée
# peu d'observations en 'polar moist' (rare) et 'boreal moist' (pas en suisse)
# cohérent car zone extrême pour les cocinelle et peu d'observateurs
# plus de coccinelle native en zone froide logique car espèce généraliste comparée à l'invasive qui est spécialiste
# cf. graphique effectuer durant l'analyse et interprétation des données
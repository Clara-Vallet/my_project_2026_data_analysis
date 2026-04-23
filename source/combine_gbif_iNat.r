###########################################################################################################################
#######                                          Préparation du terrain                                             #######
###########################################################################################################################


############################################# téléchargement des packages #################################################

# installation si nécessaire:
# install.packages(c("rgbif", "rnaturalearth", "ggplot2", "rinat","raster", "dplyr", "sf"))

# chargement des packages
library(rgbif)                                                    # accès aux données de GBIF
library(rnaturalearth)                                            # cartographie des pays
library(ggplot2)                                                  # faire les graphique
library(rinat)                                                    # accès aux données de iNaturalist
library(raster)                                                   # analyse spatiale
library(dplyr)                                                    # manipulation des tableaux
library(sf)                                                       # cartographie moderne

# faire comprendre à R de faire les calculs comme si tout était sur une surface plane (géométrie Euclidienne)
sf_use_s2(FALSE)                                                  # évite message d'erreurs



#################################################### paramètre  ############################################################

# espèces étudiées
native_species <- "Coccinella septempunctata"                     # espèce native
invasive_species <- "Harmonia axyridis"                           # espèce invasive venant d'Asie (Chine)

# nombre maximum de points à télécharger des bases de données iNat et Gbif
gbif_limit <- 5000                                                # parfois modifié directement de les scripts ci-dessous


#####################################################  Carte: Suisse   #####################################################

# téléchargement de la carte de la suisse 
Switzerland <- ne_countries(
  scale = "medium",                                               # niveau de définition                       
  returnclass = "sf",                                             # format de l'objet
  country = "Switzerland"                                         # pays ciblé
)

# visualisation préliminaire de la carte suisse vide
x11()
ggplot(data = Switzerland) +                                      # pays précédemment ciblé
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  theme_classic()                                                 # style de graphique épuré sans quadrillage 










###########################################################################################################################
#######                   Combinaison des data iNaturalist et GBif pour Coccinella septempunctata                   #######
###########################################################################################################################


######################################################  Gbif Data   #######################################################

# téléchargement des occurences brutes depuis Gbif avec coordonnées 
native_gbif_raw <- occ_data(
  scientificName = native_species,                                # nom espèce à rechercher (Coccinella septempunctata)
  hasCoordinate = TRUE,                                           # filtre que les occurences avec données géographiques
  limit = gbif_limit,                                             # nombre maximum d'occurences à télécharger (5000)
  country = "CH"                                                  # restriction aux occurences situées en suisse
)

# extraction uniquement du tableau des occurences permettant appliquer fonctions suivantes 
native_gbif_occ <- native_gbif_raw$data 

# vérification pour voir si tout est correct 
head(native_gbif_occ)                                             # affiche les 6 premières lignes du tableau
names(native_gbif_occ)                                            # affiche le nom de toutes les colonnes

# comptage du nombre d'observations globales
nrow(native_gbif_occ)                                             # N=2932 observations (< limite de 5000)

# graphique préliminaire pour voir si les points ont la forme de la suisse
x11()                                                             # ouvre sur une fenêtre à part (mac)                     
plot(
  native_gbif_occ$decimalLongitude,                               # colonne des longitudes pour axe x
  native_gbif_occ$decimalLatitude,                                # colonne latitude pour axe y
  pch = 16,                                                       # style de points
  col = "#b50072",                                              # couleur des points
  xlab = "Longitude",                                             # titre axe x
  ylab = "Latitude",                                              # titre axe y
  main = "Coccinella septempunctata in Switzerland (Gbif)"        # titre du graphique
)

# création de la belle carte complète sur le fond de la suisse (Gbif)
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland) +                                      # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_point(                                                     # ajout des points occurrences sur la carte
    data = native_gbif_occ,                                       # source des data 
    aes(x = decimalLongitude, y = decimalLatitude),               # point sur axe longitude/latitude 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#b50072",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Coccinella septempunctata (Gbif)",         # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 

# enlèver les points qui dépasse de la carte de suisse
# fond carte suisse
Switzerland_map <- st_transform(Switzerland, crs = 4326)          # carte utilise système coordonnée GPS standard 
# transformation des point en objet géographique
native_spatial_gbif <- st_as_sf(
  native_gbif_occ,                                                # donnée brute avec point qui dépassent
  coords = c("decimalLongitude", "decimalLatitude"),              # coordonnées des points
  crs = 4326)                                                     # format GPS standard (similaire à la carte)
# garder uniquement les points dans les contours suisses
native_gbif_occ_suisse <- st_intersection(        
  native_spatial_gbif, Switzerland_map)
# vérification du nettoyage
nrow(native_gbif_occ)                                             # nombre de points avant N=2932
nrow(native_gbif_occ_suisse)                                      # nombre de points après N=2852

# création de la belle carte complète sur le fond de la suisse après avoir retiré les erreus spatiales (Gbif)
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland_map) +                                  # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_sf(                                                        # ajout des points occurrences sur la carte
    data = native_gbif_occ_suisse,                                # source des data 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#b50072",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Coccinella septempunctat (Gbif)",          # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 



############################################  Nettoyage du tableau Gbif   #################################################

# création d'un beau tableau avec uniquement les colonnes qui nous intéresse
native_coords_gbif <- st_coordinates(native_gbif_occ_suisse)      # extraction des coordonnées GPS
native_data_gbif <- data.frame(
  species   = native_gbif_occ_suisse$species,                     # nom espèce 
  latitude  = native_coords_gbif[, "Y"],                          # latitude (X)
  longitude = native_coords_gbif[, "X"],                          # longitude (Y)
  date_obs  = as.Date(native_gbif_occ_suisse$eventDate),          # date simple (J-M-A) sans heure
  source    = "gbif"                                              # source des données (gbif & iNaturalist)
)

# vérification rapide du tableau 
head(native_data_gbif)                                            # affiche les 6 premières lignes du tableau
str(native_data_gbif)                                             # affiche le type de données par colonne



###################################################  iNaturalist Data   ###################################################

# téléchargement des données depuis iNaturalist
native_inat_raw <- get_inat_obs(
  maxresults=5000,                                                # limitation du nombre d'occurences téléchargées 
  query = native_species,                                         # nom espèce à rechercher (Coccinella septempunctata)
  place_id = "switzerland"                                        # restriction aux occurences situées en suisse
)

# vérification pour voir si tout est correct 
head(native_inat_raw)                                             # affiche les 6 premières lignes du tableau
names(native_inat_raw)                                            # affiche le nom de toutes les colonnes

# graphique préliminaire pour voir si les points ont la forme de la suisse
x11()                                                             # ouvre sur une fenêtre à part (mac)                     
plot(
  native_inat_raw$longitude,                                      # colonne des longitudes pour axe x
  native_inat_raw$latitude,                                       # colonne latitude pour axe y
  pch = 16,                                                       # style de points
  col = "#ff6cc9",                                              # couleur des points
  xlab = "Longitude",                                             # titre axe x
  ylab = "Latitude",                                              # titre axe y
  main = "Coccinella septempunctata in Switzerland (iNat)"        # titre du graphique
)

# création de la belle carte complète sur le fond de la suisse (iNaturalist)
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland) +                                      # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_point(                                                     # ajout des points occurrences sur la carte
    data = native_inat_raw,                                       # source des data 
    aes(x = longitude, y = latitude),                             # point sur axe longitude/latitude 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#ff6cc9",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Coccinella septempunctata (iNat)",         # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 

# enlèver les points qui dépasse de la carte de suisse
# transformation des point en objet géographique
native_spatial_inat <- st_as_sf(
  native_inat_raw,                                                # donnée brute avec point qui dépassent
  coords = c("longitude", "latitude"),                            # coordonnées des points
  crs = 4326)                                                     # format GPS standard (similaire à la carte)
# garder uniquement les points dans les contours suisses
native_inat_occ_suisse <- st_intersection(        
  native_spatial_inat, Switzerland_map)                           # map utilisé précédemment pour enlever erreur gbif
# vérification du nettoyage
nrow(native_inat_raw)                                             # nombre de points avant N=904
nrow(native_inat_occ_suisse)                                      # nombre de points après N=889

# création de la belle carte complète sur le fond de la suisse après avoir retiré les erreus spatiales (iNat)
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland_map) +                                  # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_sf(                                                        # ajout des points occurrences sur la carte
    data = native_inat_occ_suisse,                                # source des data 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#ff6cc9",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Coccinella septempunctata (iNat)",         # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 



############################################  Nettoyage du tableau iNat   #################################################

# création d'un beau tableau avec uniquement les colonnes qui nous intéresse
native_coords_inat <- st_coordinates(native_inat_occ_suisse)      # extraction des coordonnées GPS
native_data_inat <- data.frame(
  species   = native_inat_occ_suisse$scientific_name,             # nom espèce (Harmonia axyridis)
  latitude  = native_coords_inat[, "Y"],                          # latitude (X)
  longitude = native_coords_inat[, "X"],                          # longitude (Y)
  date_obs  = as.Date(native_inat_occ_suisse$observed_on),        # date simple (J-M-A) sans heure
  source    = "inat"                                              # source des données (gbif & iNaturalist)
)

# vérification rapide du tableau 
head(native_data_inat)                                            # affiche les 6 premières lignes du tableau
str(native_data_inat)                                             # affiche le type de données par colonne



###################################  Combinaison des deux tableaux iNat & Gbif   ##########################################

# empilement des tableaux iNat & Gbif
native_full_data <- bind_rows(native_data_gbif,native_data_inat)  # 'bind_rows' (bout à bout) VS 'merge' (ajout colonne)

# vérification rapide du tableau 
head(native_full_data)                                            # affiche les 6 premières lignes du tableau 
table(native_full_data$source, useNA = "ifany")                   # calcul N(gbif)=2852, N(iNat)=889 & NA=0
summary(native_full_data$date_obs)                                # résummé des data d'observation des occurrences 
# 1880 est la plus ancienne date d'observation 
# 2026 est la plus récente date d'observation
# NA=191 observations n'ont pas de date 
table(native_full_data$source[is.na(native_full_data$date_obs)])  # source des NA (date)
# observations n'ayant pas de dates viennent de gbif (e.g. musée)
# dérangeant pour faire étude temporelle donc je décide de les enlever étant donné le grand nombre d'occurence N=2932
native_full_data <- native_full_data %>%
  filter(!is.na(date_obs))                                        # enlève les observations n'ayant pas de dates

# Vérification du tableau après avoir retiré les observations sans date
nrow(native_full_data)                                            # N(total)=3550 soit N(total)=3741-N(NA)=191
summary(native_full_data$date_obs)                                # NA=0 & min/max inchangés



############################################  Graphique iNat & Gbif   #####################################################

x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland) +                                      # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_point(                                                     # ajout des points occurrences sur la carte
    data = native_full_data,                                      # source des data 
    aes(x = longitude, y = latitude, fill = source),              # point sur axe longitude/latitude 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    color = "white",                                              # couleur de la bordure des points
    alpha = 0.8
  ) +
  scale_fill_manual(                                              # choix des couleurs pour les points iNat & Gbif 
    values = c("gbif" = "#b50072", "inat" = "#ff6cc9"),       # couleurs rose pale (inat) & rose foncé (gbif)
    name = "Source des données",                                  # titre de la légende des couleurs
    labels = c("gbif" = "GBIF", "inat" = "iNaturalist")           # titre des couleurs de la légende
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Coccinella septempunctata (Gbif & iNat)",  # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 










###########################################################################################################################
#######                       Combinaison des data iNaturalist et GBif pour Harmonia axyridis                       #######
###########################################################################################################################


######################################################  Gbif Data   #######################################################

# téléchargement des occurences brutes depuis Gbif avec coordonnées 
invasive_gbif_raw <- occ_data(
  scientificName = invasive_species,                              # nom espèce à rechercher (Harmonia axyridis)
  hasCoordinate = TRUE,                                           # filtre que les occurences avec données géographiques
  limit = 10000,                                                  # nombre maximum d'occurences à télécharger
  country = "CH"                                                  # restriction aux occurences situées en suisse
)

# extraction uniquement du tableau des occurences permettant appliquer fonctions suivantes 
invasive_gbif_occ <- invasive_gbif_raw$data 

# vérification pour voir si tout est correct 
head(invasive_gbif_occ)                                           # affiche les 6 premières lignes du tableau
names(invasive_gbif_occ)                                          # affiche le nom de toutes les colonnes

# comptage du nombre d'observations globales
nrow(invasive_gbif_occ)                                           # N=6362 observations 

# graphique préliminaire pour voir si les points ont la forme de la suisse
x11()                                                             # ouvre sur une fenêtre à part (mac)                     
plot(
  invasive_gbif_occ$decimalLongitude,                             # colonne des longitudes pour axe x
  invasive_gbif_occ$decimalLatitude,                              # colonne latitude pour axe y
  pch = 16,                                                       # style de points
  col = "#f08800",                                              # couleur des points
  xlab = "Longitude",                                             # titre axe x
  ylab = "Latitude",                                              # titre axe y
  main = "Harmonia axyridis in Switzerland (Gbif)"                # titre du graphique
)

# création de la belle carte complète sur le fond de la suisse (Gbif)
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland) +                                      # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_point(                                                     # ajout des points occurrences sur la carte
    data = invasive_gbif_occ,                                     # source des data 
    aes(x = decimalLongitude, y = decimalLatitude),               # point sur axe longitude/latitude 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#f08800",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Harmonia axyridis (Gbif)",                 # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 

# enlèver les points qui dépasse de la carte de suisse
# transformation des point en objet géographique
invasive_spatial <- st_as_sf(
  invasive_gbif_occ,                                              # donnée brute avec point qui dépassent
  coords = c("decimalLongitude", "decimalLatitude"),              # coordonnées des points
  crs = 4326)                                                     # format GPS standard (similaire à la carte)
# garder uniquement les points dans les contours suisses
invasive_gbif_occ_suisse <- st_intersection(        
  invasive_spatial, Switzerland_map)
# vérification du nettoyage
nrow(invasive_gbif_occ)                                           # nombre de points avant N=6362
nrow(invasive_gbif_occ_suisse)                                    # nombre de points après N=5562

# création de la belle carte complète sur le fond de la suisse après avoir retiré les erreus spatiales (Gbif)
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland_map) +                                  # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_sf(                                                        # ajout des points occurrences sur la carte
    data = invasive_gbif_occ_suisse,                              # source des data 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#f08800",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Harmonia axyridis (Gbif)",                 # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 


 
############################################  Nettoyage du tableau Gbif   #################################################

# création d'un beau tableau avec uniquement les colonnes qui nous intéresse
invasive_coords_gbif <- st_coordinates(invasive_gbif_occ_suisse)  # extraction des coordonnées GPS
invasive_data_gbif <- data.frame(
  species   = invasive_gbif_occ_suisse$species,                   # nom espèce (Harmonia axyridis)
  latitude  = invasive_coords_gbif[, "Y"],                        # latitude (X)
  longitude = invasive_coords_gbif[, "X"],                        # longitude (Y)
  date_obs  = as.Date(invasive_gbif_occ_suisse$eventDate),        # date simple (J-M-A) sans heure
  source    = "gbif"                                              # source des données (gbif & iNaturalist)
)

# vérification rapide du tableau 
head(invasive_data_gbif)                                           # affiche les 6 premières lignes du tableau
str(invasive_data_gbif)                                            # affiche le type de données par colonne



###################################################  iNaturalist Data   ###################################################

# téléchargement des données depuis iNaturalist
invasive_inat_raw <- get_inat_obs(
  maxresults=5000,                                                # limitation du nombre d'occurences téléchargées 
  query = invasive_species,                                       # nom espèce à rechercher (Harmonia axyridis)
  place_id = "switzerland"                                        # restriction aux occurences situées en suisse
)

# vérification pour voir si tout est correct 
head(invasive_inat_raw)                                            # affiche les 6 premières lignes du tableau
names(invasive_inat_raw)                                           # affiche le nom de toutes les colonnes

# graphique préliminaire pour voir si les points ont la forme de la suisse
x11()                                                             # ouvre sur une fenêtre à part (mac)                     
plot(
  invasive_inat_raw$longitude,                                    # colonne des longitudes pour axe x
  invasive_inat_raw$latitude,                                     # colonne latitude pour axe y
  pch = 16,                                                       # style de points
  col = "#f3be78",                                              # couleur des points
  xlab = "Longitude",                                             # titre axe x
  ylab = "Latitude",                                              # titre axe y
  main = "Harmonia axyridis in Switzerland (iNat)"                # titre du graphique
)

# création de la belle carte complète sur le fond de la suisse (iNaturalist)
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland) +                                      # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_point(                                                     # ajout des points occurrences sur la carte
    data = invasive_inat_raw,                                       # source des data 
    aes(x = longitude, y = latitude),                             # point sur axe longitude/latitude 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#f3be78",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title= "Répartition Harmonia axyridis (iNat)",                # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 

# enlèver les points qui dépasse de la carte de suisse
# transformation des point en objet géographique
invasive_spatial_inat <- st_as_sf(
  invasive_inat_raw,                                              # donnée brute avec point qui dépassent
  coords = c("longitude", "latitude"),                            # coordonnées des points
  crs = 4326)                                                     # format GPS standard (similaire à la carte)
# garder uniquement les points dans les contours suisses
invasive_inat_occ_suisse <- st_intersection(        
  invasive_spatial_inat, Switzerland_map)                         # map utilisé précédemment pour enlever erreur gbif
# vérification du nettoyage
nrow(invasive_inat_raw)                                           # nombre de points avant N=3327
nrow(invasive_inat_occ_suisse)                                    # nombre de points après N=3250

# création de la belle carte complète sur le fond de la suisse après avoir retiré les erreus spatiales (iNat)
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland_map) +                                  # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_sf(                                                        # ajout des points occurrences sur la carte
    data = invasive_inat_occ_suisse,                              # source des data 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    fill = "#f3be78",                                           # couleur des points     
    color = "white"                                               # couleur de la bordure des points
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Harmonia axyridis (iNat)",                 # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 



############################################  Nettoyage du tableau iNat   #################################################

# création d'un beau tableau avec uniquement les colonnes qui nous intéresse
invasive_coords_inat <- st_coordinates(invasive_inat_occ_suisse)  # extraction des coordonnées GPS
invasive_data_inat <- data.frame(
  species   = invasive_inat_occ_suisse$scientific_name,           # nom espèce (Harmonia axyridis)
  latitude  = invasive_coords_inat[, "Y"],                        # latitude (X)
  longitude = invasive_coords_inat[, "X"],                        # longitude (Y)
  date_obs  = as.Date(invasive_inat_occ_suisse$observed_on),      # date simple (J-M-A) sans heure
  source    = "inat"                                              # source des données (gbif & iNaturalist)
)

# vérification rapide du tableau 
head(invasive_data_inat)                                          # affiche les 6 premières lignes du tableau
str(invasive_data_inat)                                           # affiche le type de données par colonne
nrow(invasive_data_inat)                                          # N(total)=3250


###################################  Combinaison des deux tableaux iNat & Gbif   ##########################################

# empilement des tableaux iNat & Gbif
invasive_full_data <- bind_rows(
  invasive_data_gbif,invasive_data_inat)                          # 'bind_rows' (bout à bout) VS 'merge' (ajout colonne)

# vérification rapide du tableau 
head(invasive_full_data)                                          # affiche les 6 premières lignes du tableau 
table(invasive_full_data$source, useNA = "ifany")                 # calcul N(gbif)=5562, N(iNat)=3250 & NA=0
summary(invasive_full_data$date_obs)                              # résummé des data d'observation des occurrences 
# 1996 est la plus ancienne date d'observation 
# 2026 est la plus récente date d'observation
# NA=64 observations n'ont pas de date 
# même problème que précédemment
invasive_full_data <- invasive_full_data %>%
  filter(!is.na(date_obs))                                        # enlève les observations n'ayant pas de dates

# Vérification du tableau après avoir retiré les observations sans date
nrow(invasive_full_data)                                          # N(total)=8748 soit N(total)=8812-N(NA)=64
summary(invasive_full_data$date_obs)                              # NA=0 & min/max inchangés



############################################  Graphique iNat & Gbif   #####################################################

x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland) +                                      # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_point(                                                     # ajout des points occurrences sur la carte
    data = invasive_full_data,                                    # source des data 
    aes(x = longitude, y = latitude, fill = source),              # point sur axe longitude/latitude 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    color = "white",                                              # couleur de la bordure des points
    alpha = 0.8
  ) +
  scale_fill_manual(                                              # choix des couleurs pour les points iNat & Gbif 
    values = c("gbif" = "#f08800", "inat" = "#f3be78"),       # couleurs orange pale (inat) & orange foncé (gbif)
    name = "Source des données",                                  # titre de la légende des couleurs
    labels = c("gbif" = "GBIF", "inat" = "iNaturalist")           # titre des couleurs de la légende
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Harmonia axyridis (Gbif & iNat)",          # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 









###########################################################################################################################
#######                                        Création du tableau général                                          #######
###########################################################################################################################

# empilement des tableaux iNat & Gbif
full_data <- bind_rows(
  invasive_full_data,native_full_data)                            # 'bind_rows' (bout à bout) VS 'merge' (ajout colonne)

# vérification rapide du tableau 
head(full_data)                                                   # affiche les 6 premières lignes du tableau 
table(full_data$source, useNA = "ifany")                          # calcul N(gbif)=8223, N(iNat)=4139  & NA=0
summary(full_data$date_obs)                                       # résummé des data d'observation des occurrences 

table(full_data$species)                                          # problème: espèce parasite (e.g. papillon)

# suppression des parasites qui n'ont rien à faire dans le tableau
full_data <- full_data %>%
  filter(species == "Harmonia axyridis" |                         # garde Harmonia axyridis
         species == "Coccinella septempunctata"                   # garde Coccinella septempunctata
)

# vérification de la suppression     
table(full_data$species)                                          # N(H. axyridis)=7989 & N(C. septempunctata)=3549 
# on voit déjà une prédominance de Harmonia axyridis
# biais possible: observation aléatoire et pas forcément réelle population entière

# vérification complète
summary(full_data)

# visualisation du tableau
x11()
View(full_data)

# visualisation globale de tout 
x11()                                                             # ouvre sur une fenêtre à part (mac)    
ggplot(data = Switzerland) +                                      # création du fond de la carte (suisse)
  geom_sf(fill = "grey95", color = "black") +                     # fond carte en gris & frontière en noir 
  geom_point(                                                     # ajout des points occurrences sur la carte
    data = full_data,                                             # source des data 
    aes(x = longitude, y = latitude, fill = species),             # point sur axe longitude/latitude 
    size = 3,                                                     # taille des points
    shape = 21,                                                   # point en cercle avec bordure
    color = "white",                                              # couleur de la bordure des points
    alpha = 0.8
  ) +
  scale_fill_manual(                                              # choix des couleurs pour les points iNat & Gbif 
    values = c("Harmonia axyridis" = "#f08800",                 # orange pour Harmonia axyridis
    "Coccinella septempunctata" = "#b50072"),                   # rose pour Coccinella septempunctata
    name = "Espèces",                                             # titre de la légende des couleurs
  ) +
  labs(                                                           # ajout des titres 
    title="Répartition Harmonia axyridis (Gbif & iNat)",          # titre principal
    x = "Longitude",                                              # titre axe x
    y = "Latitude",                                               # titre axe y
  ) +
  theme_classic()                                                 # style de graphique épuré sans quadrillage 












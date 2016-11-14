# script du jour 4 de la formation.

library(tidyverse)
reserve <- read_csv2("./reserve-assemblee-2013.csv", 
                     col_types = cols(
                       ID_SUBVENTION = col_integer(),
                       NOM_BENEFICIAIRE = col_character(),
                       DESCRIPTION_SUBVENTION = col_character(),
                       ADRESSE_BENEFICIAIRE = col_character(),
                       PROGRAMME_PJL_FINANCE = col_character(),
                       MONTANT_SUBVENTION = col_integer(),
                       PRENOM_NOM_DEPUTE_NOSDEPUTES = col_character(),
                       ID_DEPUTE_AN = col_character(),
                       ID_NOSDEPUTES = col_integer(),
                       NOM_DEPUTE_NOSDEPUTES = col_character(),
                       PRENOM_DEPUTE_NOSDEPUTES = col_character(),
                       SEXE_DEPUTE = col_character(),
                       DATE_NAISSANCE_DEPUTE = col_date(format = ""),
                       LIEU_NAISSANCE_DEPUTE = col_character(),
                       ID_DEPARTEMENT = col_character(),
                       DEPARTEMENT_NOSDEPUTES = col_character(),
                       NUMERO_CIRCONSCRIPTION = col_integer(),
                       DATE_DEBUT_MANDAT = col_date(format = ""),
                       DATE_FIN_MANDAT = col_date(format = ""),
                       MANDAT_CLOS = col_integer(),
                       GROUPE_SIGLE = col_character(),
                       PARTI_RATTACHEMENT_FINANCIER = col_character(),
                       PROFESSION = col_character(),
                       PLACE_HEMICYCLE = col_integer(),
                       URL_DEPUTE_AN = col_character(),
                       SLUG_NOSDEPUTES = col_character(),
                       URL_DEPUTE_NOSDEPUTES = col_character(),
                       API_NOSDEPUTES = col_character(),
                       NB_MANDATS = col_integer()
                     ))

library(stringr)

reserve <- reserve %>% 
  # création d'un code circo sur deux caractères
  mutate(CodeCirco = str_pad(NUMERO_CIRCONSCRIPTION, 2, "left", "0")) %>% 
  mutate(CodeCirco = paste(ID_DEPARTEMENT, CodeCirco, sep = "-")) # concaténation

# barplot classique
reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(stat = "count")

# barplot stacked avec couleur par groupe politique
reserve %>% 
  ggplot(aes(x = 1)) +
  geom_bar(aes(fill = GROUPE_SIGLE), stat = "count", position = position_fill())

# thème xkcd !

library(xkcd)
reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(stat = "count") +
  theme_xkcd()

# on inverse les axes
reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(stat = "count") +
  coord_flip()

# on mappe la variable GROUPE_SIGLE à deux aesthetics
reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(aes(fill = GROUPE_SIGLE), stat = "count")
  
# changement de thème
reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(stat = "count") +
  theme_bw()

# changement de thème
reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(stat = "count", fill = "green") +
  theme_bw()

# changement de scale
reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(aes(fill = GROUPE_SIGLE), stat = "count") +
  theme_bw() +
  scale_fill_brewer("Paired", type = "qual")

# changement de scale à la main
reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(aes(fill = GROUPE_SIGLE), stat = "count") +
  theme_bw() +
  scale_fill_manual(values = c("ECOLO" = "dark green",
                               "GDR" = "dark red",
                               "NI" = "grey",
                               "RRDP" = "orange",
                               "SRC" = "pink",
                               "UDI" = "light blue",
                               "UMP" = "dark blue"))

# stocker les valeurs
# scale_groupes <- scale_fill_manual(values = c("ECOLO" = "dark green",
#                              "GDR" = "dark red",
#                              "NI" = "grey",
#                              "RRDP" = "orange",
#                              "SRC" = "pink",
#                              "UDI" = "light blue",
#                              "UMP" = "dark blue"))

source("./scale_groupes.R", local = TRUE)

reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(aes(fill = GROUPE_SIGLE), stat = "count") +
  theme_bw() +
  scale_groupes


# exporter un graphique

pdf(file = "./mongraphique.pdf", width = 10, height = 10)

reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE)) +
  geom_bar(aes(fill = GROUPE_SIGLE), stat = "count") +
  theme_bw() +
  scale_groupes

dev.off()

# graphiques bivariés

reserve %>% 
  group_by(GROUPE_SIGLE) %>% 
  summarise(valeur = mean(MONTANT_SUBVENTION)) %>% 
  filter(!is.na(GROUPE_SIGLE)) %>% 
  ggplot(aes(x = GROUPE_SIGLE, y = valeur)) +
  geom_bar(stat = "identity")

# facet

reserve %>% 
  ggplot(aes(x = MONTANT_SUBVENTION)) +
  geom_histogram()

reserve %>% 
  ggplot(aes(x = MONTANT_SUBVENTION)) +
  geom_histogram(bins = 100)


# changer l'échelle

reserve %>% 
  ggplot(aes(x = MONTANT_SUBVENTION)) +
  geom_histogram(bins = 30) +
  scale_x_log10(labels =  scales::comma) 
  
# facet par groupe parlementaire

reserve %>% 
  ggplot(aes(x = MONTANT_SUBVENTION)) +
  geom_histogram(bins = 30) +
  scale_x_log10(labels =  scales::comma) +
  facet_wrap(~ GROUPE_SIGLE, scales = "free_y")

# que sont les députés NA ?

reserve %>% 
  filter(is.na(GROUPE_SIGLE)) %>% 
  distinct(NOM_DEPUTE_NOSDEPUTES)

reserve %>% 
  filter(is.na(GROUPE_SIGLE)) %>% 
  distinct(NOM_BENEFICIAIRE)

# paramètre alpha

reserve %>% 
  ggplot(aes(x = DATE_NAISSANCE_DEPUTE, y = MONTANT_SUBVENTION)) +
  geom_point(alpha = 0.2) +
  scale_y_log10() +
  theme_bw()

# représenter la densité des points

reserve %>% 
  ggplot(aes(x = DATE_NAISSANCE_DEPUTE, y = MONTANT_SUBVENTION)) +
  geom_hex() +
  scale_y_log10() +
  scale_fill_continuous(low = "white", high = "dark green") +
  theme_bw() 

# croiser trois variables
reserve %>% 
  ggplot(aes(x = DATE_NAISSANCE_DEPUTE, y = MONTANT_SUBVENTION)) +
  geom_point(aes(colour = GROUPE_SIGLE), alpha = 0.8) +
  scale_y_log10() +
  theme_bw()

# geom_density
reserve %>% 
  ggplot(aes(x = MONTANT_SUBVENTION)) +
  geom_density() +
  scale_x_log10(labels =  scales::comma) 

# geom_smooth
reserve %>% 
  ggplot(aes(x = DATE_NAISSANCE_DEPUTE, y = MONTANT_SUBVENTION)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  scale_y_log10() +
  theme_bw()

reserve %>% 
  ggplot(aes(x = DATE_NAISSANCE_DEPUTE, y = MONTANT_SUBVENTION)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  scale_y_log10() +
  theme_bw()

reserve %>% 
  ggplot(aes(x = GROUPE_SIGLE, y = MONTANT_SUBVENTION)) +
  geom_boxplot()

# chargement des packages utiles

library(tidyverse)
library(stringr)

# Chargement des données 

bvINSEE <- read_csv2("./data/bvINSEE2012.csv")
# les identifiants des bureaux de vote sont codés différemment dans les deux jeux de données, on harmonise cela
bvINSEE <- bvINSEE %>% 
  mutate(BUREAU_ID = str_replace(BUREAU_ID, "_", ""))
marseille <- read_csv("./data/complet_par_bureaux.csv", 
                      col_types = cols(
                        bdv = col_character(),
                        inscrits = col_integer(),
                        Nuls = col_integer(),
                        `ALLIANCE ECOLOGISTE INDEPENDANTE` = col_integer(),
                        `CHANGER LA DONNE` = col_integer(),
                        `LISTE D UNITE ET DE RESISTANCE CONTRE LA POLITIQUE DU GOUVERNEMENT ET DE L U.E SOUTENUE PAR LE POI` = col_integer(),
                        `LUTTE OUVRIERE FAIRE ENTENDRE LE CAMP DES TRAVAILLEURS` = col_integer(),
                        `MARSEILLE A GAUCHE, L HUMAIN D ABORD DANS NOTRE VILLE` = col_integer(),
                        `MARSEILLE A VIVRE` = col_integer(),
                        `MARSEILLE BLEU MARINE` = col_integer(),
                        `MARSEILLE EN AVANT AVEC JEAN-CLAUDE GAUDIN` = col_integer(),
                        `MARSEILLE ENSEMBLE` = col_integer(),
                        `MARSEILLE J Y CROIS` = col_integer(),
                        `MARSEILLE POPULAIRE` = col_integer(),
                        `MARSEILLE UNIE` = col_integer(),
                        `UN NOUVEAU CAP POUR LES MARSEILLAIS AVEC PATRICK MENNUCCI` = col_integer(),
                        `UNE QUALITE DE VIE POUR TOUS` = col_integer(),
                        `UNION POUR MARSEILLE` = col_integer(),
                        `Total Résultat` = col_integer()
                      ))


# Nettoyage des données

marseille <- marseille %>%
  mutate( # on met les numéros de BV au bon format
    BUREAU_ID = paste0("132", str_sub(str_pad(bdv, 4, "left", "0"), 1, 2), str_pad(bdv, 4, "left", "0")),
    Inscrits = inscrits,
    Abstention = inscrits - Nuls - `Total Résultat`,
    Exprimés = `Total Résultat`,
    Diouf = `CHANGER LA DONNE`,
    AEI = `ALLIANCE ECOLOGISTE INDEPENDANTE`,
    POI = `LISTE D UNITE ET DE RESISTANCE CONTRE LA POLITIQUE DU GOUVERNEMENT ET DE L U.E SOUTENUE PAR LE POI`,
    Coppola = `MARSEILLE A GAUCHE, L HUMAIN D ABORD DANS NOTRE VILLE`,
    Assante = `MARSEILLE A VIVRE`,
    Ravier = `MARSEILLE BLEU MARINE`,
    Gaudin = `MARSEILLE EN AVANT AVEC JEAN-CLAUDE GAUDIN`,
    MarseilleEnsemble = `MARSEILLE ENSEMBLE`,
    Persia = `MARSEILLE J Y CROIS`,
    MarseillePopulaire = `MARSEILLE POPULAIRE`,
    MarseilleUnie = `MARSEILLE UNIE`,
    Mennucci = `UN NOUVEAU CAP POUR LES MARSEILLAIS AVEC PATRICK MENNUCCI`,
    Qualite = `UNE QUALITE DE VIE POUR TOUS`,
    UnionMarseille = `UNION POUR MARSEILLE`
  )
# quand une cellule est vide : mettre une valeur égale à zéro
marseille[is.na(marseille)] <- 0

# On transforme les résultats en % des inscrits

marseille <- marseille %>% 
  mutate_at(vars(Nuls, Abstention, Diouf, Coppola, Ravier, Gaudin, Mennucci), 
            funs(. / Inscrits * 100))

# on fusionne les résultats électoraux avec les données de l'INSEE

marseille <- left_join(marseille, bvINSEE, by = "BUREAU_ID")

# on transforme les données INSEE en %

marseille <- marseille %>%
  mutate(CS1 = C09_ACT1564_CS1 / P09_POP1564 * 100,
         CS2 = C09_ACT1564_CS2 / P09_POP1564 * 100,
         CS3 = C09_ACT1564_CS3 / P09_POP1564 * 100,
         CS4 = C09_ACT1564_CS4 / P09_POP1564 * 100,
         CS5 = C09_ACT1564_CS5 / P09_POP1564 * 100,
         CS6 = C09_ACT1564_CS6 / P09_POP1564 * 100,
         etrangers = P09_POP_ETR / P09_POP * 100,
         chomage = P09_CHOM1564 / P09_POP1564 * 100,
         HLM = P09_NPER_RP_LOCHLMV / P09_POP * 100)

# enfin, on estime le modèle

modele1 <- lm(Ravier ~ CS2 + CS3 + CS4 + CS5 + CS6 + etrangers + chomage + HLM, data = marseille)

# on regarde les résultats du modèle

summary(modele1)

# on regarde les résultats du modèle, avec une interface plus lisible

# install.packages("texreg")
library(texreg)
# l'option single.row permet de faire tenir le coefficient et son erreur-type sur la même ligne
screenreg(modele1)


# on peut ensuite analyser un élément donné du modèle, par exemple les résidus

summary(modele1$residuals)
plot(density(modele1$residuals))
# une altenative avec ggplot2
library(ggplot2)
# fortify permet de transformer l'objet lm en dataframe utilisable par ggplot2
ggmodele1 <- fortify(modele1)
ggplot(modele1, aes(x=.resid)) + 
  geom_density(color="blue") + 
  theme_bw()

# ou encore les valeurs prédites (fitted values), par exemple 
# on charge le fonds de carte de Marseille
load("./data/marseilleSHP.Rdata")
# on prépare le jeu de données pour ggplot2
library(maptools)
ggmarseilleSHP <- fortify(marseilleSHP, region="ID")
ggmarseilleSHP$pred <- modele1$fitted.values[match(ggmarseilleSHP$id, marseille$BUREAU_ID)]

# on prépare le thème

new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")

ggplot(ggmarseilleSHP, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=pred)) + 
  coord_fixed() + 
  scale_fill_gradient(guide="legend", name="Valeurs prédites", low="white", high="brown") + 
  labs(x=NULL, y=NULL) + 
  new_theme_empty


# même carte mais avec les résidus
ggmarseilleSHP$res <- modele1$residuals[match(ggmarseilleSHP$id, marseille$BUREAU_ID)]
ggplot(ggmarseilleSHP, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=res)) + 
  coord_fixed() + 
  scale_fill_gradient2(guide="legend", name="Résidus", low="blue", high="red", mid="white") + 
  labs(x=NULL, y=NULL) + 
  new_theme_empty

# nouveau modèle, avec effets d'interaction

modele2 <- lm(Ravier ~ CS2 + CS3 + CS4 + CS5 + CS6*etrangers + CS6*chomage + HLM, data = marseille)
screenreg(list(modele1, modele2))

## attention : ce modèle présente une forte multicollinéarité !
library(car)
vif(modele2)

newdata <- expand.grid(CS6 = quantile(marseille$CS6, probs=seq(0,1,length.out=10), na.rm=TRUE), CS2 = mean(marseille$CS2, na.rm=TRUE), CS3 = mean(marseille$CS3, na.rm=TRUE), CS4 = mean(marseille$CS4, na.rm=TRUE), CS5 = mean(marseille$CS5, na.rm=TRUE), etrangers = quantile(marseille$etrangers, probs=seq(0,1,0.25), na.rm=TRUE), chomage = mean(marseille$chomage, na.rm=TRUE), HLM = mean(marseille$HLM, na.rm=TRUE))
newdata$predict <- predict(modele2, newdata=newdata)

newdata %>%
  ggplot(aes(x = CS6, y = predict, group = etrangers)) +
  geom_line(aes(color = as.factor(etrangers))) +
  scale_color_discrete(name = "proportion d'étrangers", labels = c("minimum", "1er quartile", "2e quartile", "3e quartile", "maximum")) +
  xlab("Ouvriers") +
  ylab("Score FN prédit") +
  theme_bw()

# modèle avec prise en compte des arrondissements

marseille$arrdt <- substr(marseille$BUREAU_ID, 4,5)
# install.packages("lme4")
library(lme4)
modele3 <- lmer(Ravier ~ CS2 + CS4 + CS5 + CS6 + etrangers + chomage + HLM + (1 | arrdt), data = marseille)

screenreg(list(modele1, modele2, modele3))

modele4 <- lmer(Ravier ~ CS2 + CS3 + CS4 + CS5 + HLM + (0 + CS6 | arrdt), data = marseille)

screenreg(list(modele1, modele2, modele3, modele4))

modele5 <- lmer(Ravier ~ CS2 + CS3 + CS4 + CS5 + HLM + (1 + CS6 | arrdt), data = marseille)

screenreg(list(modele1, modele2, modele3, modele4, modele5))

ranef(modele3)
ranef(modele4)

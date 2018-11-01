library("tidyverse")

library("FactoMineR")
library("factoextra") #http://www.sthda.com/english/rpkgs/factoextra/

library("missMDA")
library("VIM")

# https://www.youtube.com/watch?v=OOM8_FH6_8o&feature=youtu.be
# http://factominer.free.fr/course/missing.html

# Lecture des données, on ne garde que l'année 2010 :
raw_data2 <- readxl::read_xlsx("data/Popular Indicators.xlsx", na="..") %>% filter(Time==2010)

# On calcul la proportion de missing dans chaque colonne :
raw_data2 %>%
  summarise_all(funs(sum(is.na(.)))) / nrow(raw_data2) * 100 -> missing_values_pct

# On peut trier de la colonne avec le plus de valeurs manquantes à celle qui
# en a le moins :
missing_values_pct <- missing_values_pct %>% gather() %>% arrange(-value)

# On ne garde que les colonnes dont la proportion de valeurs manquantes
# est inférieure à 20%.
# On pourra essayer une valeur plus élevée plus tard :
inf_20_pct<-missing_values_pct %>% filter(value<=20)

# En reprenant le nom des colonnes ayant peu de valeurs manquantes,
# on fait un df avec un nombre réduit de colonnes :
data_limited_missing <- raw_data2 %>% select(inf_20_pct$key)

# On drop les lignes ayant une valeur manquante dans la colonne Fertility rate :
data_clean <- data_limited_missing %>% drop_na(`Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]`)

# Divers plots :
VIM::aggr(data_clean, plot=F)

VIM::aggr(data_clean)

ggplot(data = data_clean) +
  geom_histogram(mapping = aes(x = `Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]`))

ggplot(data = data_clean, mapping = aes(x = `Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]`,
                                        y = `Adolescent fertility rate (births per 1,000 women ages 15-19) [SP.ADO.TFRT]`)) + 
  geom_smooth(se = TRUE) +
  geom_point()+
  labs(title="Total fertility vs. Ado fertility", x="Total fertility", y="Ado fertility")

ggplot(data = data_clean, mapping = aes(x = `Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]`,
                                        y = `GNI per capita, PPP (current international $) [NY.GNP.PCAP.PP.CD]`)) + 
  geom_smooth(se = TRUE) +
  geom_point()+
  labs(title="Total fertility vs. Wealth", x="Total fertility", y="GNI per capita ($)")

ggplot(data = data_clean, mapping = aes(x = `Fertility rate, total (births per woman) [SP.DYN.TFRT.IN]`,
                                        y = `Mobile cellular subscriptions (per 100 people) [IT.CEL.SETS.P2]`)) + 
  geom_smooth(se = TRUE) +
  geom_point()+
  labs(title="Total fertility vs. Mobile subscriptions per 100 people",
       x="Total fertility",
       y="Mobile subscriptions per 100 people")

# avec missMDA, on réalise une imputation multiple pour
# mesurer l'incertitude sur les valeurs imputées :
data_clean_numeric <- data_clean %>% select(which(sapply(.,is.numeric))) %>% as.data.frame(.)
nbdim <- estim_ncpPCA(data_clean_numeric) # dans un premier temps tout le df après on exclut la variable cible ?
res.comp <- MIPCA(data_clean_numeric, ncp = nbdim$ncp, scale=TRUE, nboot = 100)

plot(res.comp)

# Le tableau avec les valeurs imputées est disponible comme ceci :
data_imputed <- res.comp$res.imputePCA
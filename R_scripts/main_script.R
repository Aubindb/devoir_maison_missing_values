library("tidyverse")
library("magrittr")

library("FactoMineR")
library("factoextra") #http://www.sthda.com/english/rpkgs/factoextra/

library("missMDA")
library("mice")
library("randomForest")
library("FactoInvestigate")

library("VIM")

library(UpSetR)
library(naniar) #https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

# https://www.youtube.com/watch?v=OOM8_FH6_8o&feature=youtu.be
# http://factominer.free.fr/course/missing.html

# Lecture des données, on ne garde que l'année 2010 :
raw_data2 <- readxl::read_xlsx("data/Popular Indicators.xlsx", na="..") %>% filter(Time==2010)

# On calcul la proportion de missing dans chaque colonne :
raw_data2 %>%
  summarise_all(funs(sum(is.na(.)))) / nrow(raw_data2) * 100 -> missing_values_pct
#Graphiquement 
gg_miss_var(raw_data2, show_pct = TRUE)

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

# On va dropper les colonnes de mesures de richesses à l'échelle
# d'un pays qui peuvent être corrélés aux mesures par tête :
data_clean %<>% select(-c(`GNI, PPP (current international $) [NY.GNP.MKTP.PP.CD]`,
                          `GNI, Atlas method (current US$) [NY.GNP.ATLS.CD]`,
                          `GDP (current US$) [NY.GDP.MKTP.CD]`))

# On renomme les colonnes avec des lettres :
saved_names <- names(data_clean)
LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
names(data_clean) <- LETTERS702[1:dim(data_clean)[2]]

# On créé un dictionnaire pour conserver la correspondance entre
# les lettres et le nom des variables d'origine
letters_dict <- saved_names
names(letters_dict) <- LETTERS702[1:dim(data_clean)[2]]

# Divers plots :
VIM::aggr(data_clean, plot=F)

VIM::aggr(data_clean)

gg_miss_upset(data_clean, nsets = n_var_miss(data_clean))

ggplot(data = data_clean) +
  geom_histogram(mapping = aes(x = Q))

ggplot(data = data_clean, mapping = aes(x = Q,
                                        y = N)) + 
  geom_smooth(se = TRUE) +
  geom_point()+
  labs(title="Total fertility vs. Ado fertility", x="Total fertility", y="Ado fertility")

ggplot(data = data_clean, mapping = aes(x = Q,
                                        y = `F`)) + 
  geom_smooth(se = TRUE) +
  geom_point()+
  labs(title="Total fertility vs. Wealth", x="Total fertility", y="GNI per capita ($)")

ggplot(data = data_clean, mapping = aes(x = Q,
                                        y = U)) + 
  geom_smooth(se = TRUE) +
  geom_point()+
  labs(title="Total fertility vs. Mobile subscriptions per 100 people",
       x="Total fertility",
       y="Mobile subscriptions per 100 people")



# avec missMDA, on réalise une imputation multiple pour
# mesurer l'incertitude sur les valeurs imputées :
data_clean_numeric <- data_clean %>%
  select(which(sapply(.,is.numeric))) %>%
  as.data.frame(.)

# ajout 3/11 : centrage et réduction des données, conseillé avant PCA :
data_clean_numeric <- scale(data_clean_numeric) %>% as.data.frame(.)
# On sort la variable cible :
target <- data_clean_numeric$Q
data_clean_numeric %<>% select(-Q)
# Premier mode opératoire :
# on réalise une imputation multiple pour évaluer la pertinence de la methode
# puis on utilise imputePCA pour avoir un PCA sur tableau complet 
# on pourra alors essayer de fitter un modèle linéaire sur l'output.

nbdim <- estim_ncpPCA(data_clean_numeric) # dans un premier temps tout le df après on exclut la variable cible ?
res.comp <- MIPCA(data_clean_numeric, ncp = nbdim$ncp, scale=TRUE, nboot = 10)
imp<-prelim(res.comp, data_clean_numeric)
pdf("output/missMDA/plots.pdf")
densityplot(imp)
stripplot(imp, pch = 20, cex = 1.2)
dev.off()

#pdf("output/missMDA/MIPCA-100.pdf")
plot(res.comp, cex.lab=.5)
#dev.off()
# Le tableau avec les valeurs imputées est disponible comme ceci :
data_imputed <- res.comp[["res.imputePCA"]]

res<-PCA(data_imputed)
plot(res)

fviz_screeplot(res, addlabels = TRUE, ylim = c(0, 35))
# Control variable colors using their contributions
fviz_pca_var(res, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Avoid text overlapping)

# Représentation des individus : graphique peu lisible :
#fviz_pca_ind(res, col.ind = "cos2",
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#             repel = TRUE) # Avoid text overlapping (slow if many points)


# Les contributions des variables à l'axe 1
fviz_contrib(res, choice = "var", axes = 1, top = 10)
# Les contributions des variables à l'axe 2
fviz_contrib(res, choice = "var", axes = 2, top = 10)

fviz_pca_ind(res,
             label = "none", # hide individual labels
             habillage = cut(target,3), # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)

Investigate(res)
# modélisation avec 5 dimensions
ind_coord <- res[["ind"]][["coord"]] %>% as_tibble()
ind_coord_target <- ind_coord %>% mutate(target = target)
fit <- lm(target~.-1, ind_coord_target)
summary(fit)
plot(fit)

# Second mode opératoire avec mice. On va utiliser plusieurs méthodes
# offertes par le package afin de compléter les valeurs manquantes
# et de construire un modèle :
data_clean_numeric_mice <- data_clean %>% select(which(sapply(.,is.numeric)))
data_clean_numeric_mice <- scale(data_clean_numeric_mice) %>% as.data.frame(.)
# On sort la variable cible. Etant donné le fonctionnement particulier
# de pool, il est difficile d'ajouter une colonne "target" avant le modeling
# comme nous l'avons fait avec missMDA. Cependant, l'utilisation
# du paramètre predictorMatrix nous permet d'exclure une variable
# lors de l'imputation :
imp <- mice(data_clean_numeric_mice, print = FALSE)
pred <- imp$predictorMatrix
pred[, "Q"] <- 0

# pmm : predictive mean matching : pb > la matrice n'est pas inversible
# voir https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586

#random forest
imputed_rf = mice(data_clean_numeric_mice, pred = pred,method="rf", m=5)
pdf("output/mice/random_forest/plots.pdf")
densityplot(imputed_rf)
stripplot(imputed_rf, pch = 20, cex = 1.2)
dev.off()

#Classification and regression trees
imputed_cart = mice(data_clean_numeric_mice, pred = pred, method="cart", m=5)
pdf("output/mice/cart/plots.pdf")
densityplot(imputed_cart)
stripplot(imputed_cart, pch = 20, cex = 1.2)
dev.off()

#Unconditional mean imputation
imputed_mean = mice(data_clean_numeric_mice, pred = pred, method="mean", m=1)
pdf("output/mice/mean/plots.pdf")
densityplot(imputed_mean)
stripplot(imputed_mean, pch = 20, cex = 1.2)
dev.off()

#Linear regression ignoring model error
imputed_normnob = mice(data_clean_numeric_mice, pred = pred, method="norm.nob", m=10)
pdf("output/mice/norm_nob/plots.pdf")
densityplot(imputed_normnob)
stripplot(imputed_normnob, pch = 20, cex = 1.2)
dev.off()

#Predictive mean matching
imputed_pmm = mice(data_clean_numeric_mice, pred = pred, method="pmm", m=10)
pdf("output/mice/pmm/plots.pdf")
densityplot(imputed_pmm)
stripplot(imputed_pmm, pch = 20, cex = 1.2)
dev.off()

# Début du modeling : Q~. ne fonctionne pas !
fit <- with(data = imputed_pmm, exp = lm(Q ~ A+B+C+D+E+F+G+H+I+J+
                                           K+L+M+O+P+R+S+T+U+V+AA))

mod_step <- step(fit$analyses[[3]], direction = "both")
summary(mod_step)
summary(pool(fit))
summary(fit$analyses[[3]])

fit <- with(data = imputed_rf, exp = lm(Q ~ A+B+C+D+E+F+G+H+I+J+
                                           K+L+M+O+P+R+S+T+U+V+AA))
summary(pool(fit))

# Selection de variable avec la méthode conseillée par van burren
# le sujet semble assez délicat.

step_on_mice <- function (mice_object, direction = "both") {
  fit <- with(data = mice_object, exp = lm(Q ~ A+B+C+D+E+F+G+H+I+J+
                                             K+L+M+O+P+R+S+T+U+V+AA))
  len <- length(fit$analyses)
  all_kept_var <- vector()
  for (i in 1:len) {
    mod_step <- step(fit$analyses[[i]], direction = direction)
    kept_var <- names(mod_step[["coefficients"]])
    all_kept_var <- c(all_kept_var, kept_var)
  }
  table(all_kept_var)
}

make_linear_model <- function(mice_object, step_on_mice_res, threshold = 5, intercept=TRUE){
  final_var <- step_on_mice_res[step_on_mice_res > threshold]
  fin_var_name <- names(final_var)
  fin_var_name <- fin_var_name[fin_var_name!= "(Intercept)"]
  if (intercept){
  formula_ <- as.formula(paste("Q", paste(fin_var_name, collapse=" + "), sep=" ~ "))
  } else {
    formula_ <- as.formula(paste("Q", paste(paste(fin_var_name, collapse=" + "), "-1"), sep=" ~ "))
  }
  print(formula_)
  with(data = mice_object, exp=lm(formula(format(formula_))))
}
var_count <- step_on_mice(imputed_pmm)
plot(var_count)
fit_selected_var <- make_linear_model(imputed_pmm, var_count)
summary(pool(fit_selected_var))
# plot

plot_imputed <- function(mice_object, original_dataset, column, plot_type="model", se=F, method="auto"){
  x_imputations <- mice_object[["imp"]][[column]]
  index <- row.names(x_imputations[1])
  ordo <- original_dataset[["Q"]][strtoi(index)]
  imputed_points <- x_imputations %>%
    as_tibble() %>%
    gather() %>%
    mutate(ordo=rep(ordo, length(x_imputations)))
  if (plot_type=="simple") {
    ggplot() +
      geom_point(data = original_dataset, aes_string(y = "Q", x = column)) +
      geom_point(data = imputed_points, aes(y = ordo, x = value), colour="#CC0000", alpha = 0.5) +
      xlab(letters_dict[column]) + ylab("Fertility rate, births per woman")
  } else if (plot_type=="model"){
    ggplot() +
      geom_point(data = original_dataset, aes_string(y = "Q", x = column)) +
      geom_smooth(data = original_dataset, aes_string(y = "Q", x = column), se=se, method=method) +
      geom_point(data = imputed_points, aes(y = ordo, x = value), colour="#CC0000", alpha = 0.5) +
      geom_smooth(data = imputed_points, aes(y = ordo, x = value), colour="#CC0000", alpha = 0.5, se=se, method=method) +
      xlab(letters_dict[column]) + ylab("Fertility rate, births per woman")
  }

}
plot_imputed(imputed_pmm, data_clean_numeric_mice, "A", method = "auto")
plot_imputed(imputed_pmm, data_clean_numeric_mice, "AA", method = "auto")
plot_imputed(imputed_pmm, data_clean_numeric_mice, "D", method = "auto")
plot_imputed(imputed_pmm, data_clean_numeric_mice, "K", method = "auto")
plot_imputed(imputed_pmm, data_clean_numeric_mice, "M", method = "auto")
plot_imputed(imputed_pmm, data_clean_numeric_mice, "P", method = "auto")
plot_imputed(imputed_pmm, data_clean_numeric_mice, "S", method = "auto")
plot_imputed(imputed_pmm, data_clean_numeric_mice, "T", method = "auto")
plot_imputed(imputed_pmm, data_clean_numeric_mice, "U", method = "auto")

plot_imputed(imputed_normnob, data_clean_numeric_mice, "A", method = "auto")
plot_imputed(imputed_normnob, data_clean_numeric_mice, "AA", method = "auto")
plot_imputed(imputed_normnob, data_clean_numeric_mice, "D", method = "auto")
plot_imputed(imputed_normnob, data_clean_numeric_mice, "K", method = "auto")
plot_imputed(imputed_normnob, data_clean_numeric_mice, "M", method = "auto")
plot_imputed(imputed_normnob, data_clean_numeric_mice, "P", method = "auto")
plot_imputed(imputed_normnob, data_clean_numeric_mice, "S", method = "auto")
plot_imputed(imputed_normnob, data_clean_numeric_mice, "T", method = "auto")
plot_imputed(imputed_normnob, data_clean_numeric_mice, "U", method = "auto")

som_res <- step_on_mice(imputed_pmm)
make_linear_model(imputed_pmm, som_res) %>% summary()

# Une fois que nous avons choisi 

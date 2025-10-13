###### 
## written by Hanna Haf
## 13 October 2025

###### libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)  # für tidy()
library(writexl)
library(ggpubr)



###### load and prepare data
###Datensatz Umgestellt
ds2 <- read_excel("data/20251013_CSASustainability_dataset2.xlsx")

### Datenbereinigung 

### Ausschluss Solawi-Umstellung 

colnames(ds2)[6] <- "Umstellung"
ds2 <- ds2 [!is.na(ds2$Umstellung) & ds2$Umstellung == "Ja (weiter mit der Umfrage)", ]

## wir löschen alles raus außer (die Umstellungsfrage und) die Nachhaltigkeitsfragen 

ds2 <- ds2 [, c(50: 99)]


##Umbenennung der Spalten
names(ds2) <- c(
   "Crop diversity", "Creation of habitats", "Form of plant protection", "Livestock diversity", "Grassland management", "Animal welfare", 
   "Energy consumption", "Fuel consumption", "Use of renewable energies", "Material consumption", "Transport and traffic", "Water usage", "Land use and animal husbandry", "Food losses", "Form of fertilization", "Form of soil cultivation", "Crop rotation",
   "Wages and profits", "Workplace quality", "Involvement of employees", "Number of business branches", "Proportion of land owned by the farm", "Reliable turnover", "Liquidity for reserves or equity capital", "Dependence on subsidies", "Remuneration for ecosystem services provided", "Closed farm cycle", 
   "Increase regional marketing", "Regional purchases of production resources", "Creation of regional jobs", "Expansion of business cooperation", "Expansion of social cooperation",
   "Educational programs", "Transfer of specialist knowledge", "Inclusion and integration", "Transparency", "Participation", "Preservation of cultural heritage",
   "Environmental consciousness", "Sustainability behavior", "Social cohesion members", "Social support on the farm", "Proximity to agriculture", "Health", "Quality of life", "Civic engagement", "Supply stability", "Food prices", "Willingness to pay", "Time spent on food purchasing")
  

# Alle VPN löschen, die bei ALLEN Skalen NA haben (also in allen Spalten außer "Umstellung")
ds2 <- ds2[!apply(ds2[ , -1], 1, function(x) all(is.na(x))), ]
# View(ds2)

# Umcodieren der Textantworten in numerische Werte
ds2 <- ds2 %>%
  mutate(across(everything(), ~ dplyr::recode(.,
                                              "negative Effekte"      = -2,
                                              "eher negative Effekte" = -1,
                                              "neutrale Effekte"      =  0,
                                              "eher positive Effekte" =  1,
                                              "positive Effekte"      =  2,
                                              "nicht relevant"        = 0 ## kann auch als neutral geframet werden
  )))


# View(ds2)

#### add group name
ds2$Gruppe <- "Converted farms"


### DATENSATZ UMSTELLUNGSBEREIT 

ds_b <- read_excel("data/20251013_CSASustainability_dataset1.xlsx")

### Datenbereinigung 

unique(ds_b[[5]])

names(ds_b)[5] <- "Name"
## Gruppennamen  hinzufügen
ds_b$Gruppe <- "Farms interested in conversion"


## wir löschen alles raus außer (die Umstellungsfrage und) die Nachhaltigkeitsfragen 
names(ds_b)
ds_b <- ds_b [, c(6: 55,60)]

##Umbenennung der Spalten
names(ds_b) <- c(
  "Crop diversity", "Creation of habitats", "Form of plant protection", "Livestock diversity", "Grassland management", "Animal welfare", 
  "Energy consumption", "Fuel consumption", "Use of renewable energies", "Material consumption", "Transport and traffic", "Water usage", "Land use and animal husbandry", "Food losses", "Form of fertilization", "Form of soil cultivation", "Crop rotation",
  "Wages and profits", "Workplace quality", "Involvement of employees", "Number of business branches", "Proportion of land owned by the farm", "Reliable turnover", "Liquidity for reserves or equity capital", "Dependence on subsidies", "Remuneration for ecosystem services provided", "Closed farm cycle", 
  "Increase regional marketing", "Regional purchases of production resources", "Creation of regional jobs", "Expansion of business cooperation", "Expansion of social cooperation",
  "Educational programs", "Transfer of specialist knowledge", "Inclusion and integration", "Transparency", "Participation", "Preservation of cultural heritage",
  "Environmental consciousness", "Sustainability behavior", "Social cohesion members", "Social support on the farm", "Proximity to agriculture", "Health", "Quality of life", "Civic engagement", "Supply stability", "Food prices", "Willingness to pay", "Time spent on food purchasing",
  "Gruppe")

# View(ds_b)

# Zeilennummerierung ab 1:
rownames(ds_b) <- NULL

# Datensätze zusammenführen
ds_all <- bind_rows(ds2, ds_b)


### Hilfsfunktion: Mittelwert bilden, NaN -> NA
row_mean_na <- function(df) {
  m <- rowMeans(df, na.rm = TRUE)
  m[rowSums(!is.na(df)) == 0] <- NA_real_
  m
}


### Bildung der Skalen in neuem Datensatz
ds_scales <- ds_all %>%
  mutate(
    Biodiversity = row_mean_na(select(., `Crop diversity`,`Creation of habitats`, `Form of plant protection`,
                                      `Livestock diversity`, `Grassland management`, `Animal welfare`)),
    
    `Climate Water` = row_mean_na(select(., `Energy consumption`, `Fuel consumption`,
                                       `Use of renewable energies`, `Material consumption`, 
                                       `Transport and traffic`, `Water usage`,
                                       `Land use and animal husbandry`, `Food losses`)),
    
    `Soil fertility` = row_mean_na(select(., `Form of fertilization`, `Form of soil cultivation`, `Crop rotation`)),
    
    `Working conditions` = row_mean_na(select(., `Wages and profits`, `Workplace quality`, `Involvement of employees`)),
    
    `Farm economy` = row_mean_na(select(., `Number of business branches`,
                                      `Proportion of land owned by the farm`,
                                      `Reliable turnover`, `Liquidity for reserves or equity capital`,
                                      `Dependence on subsidies`, `Remuneration for ecosystem services provided`,
                                      `Closed farm cycle`)),
    
    `Regional economy` = row_mean_na(select(., `Increase regional marketing`, `Regional purchases of production resources`,
                                          `Creation of regional jobs`, `Expansion of business cooperation`, 
                                          `Expansion of social cooperation`)),
    
    Society = row_mean_na(select(., `Educational programs`, `Transfer of specialist knowledge`, 
                                 `Inclusion and integration`, `Transparency`, `Participation`, 
                                 `Preservation of cultural heritage`)),
    
    Members = row_mean_na(select(., `Environmental consciousness`, `Sustainability behavior`, `Social cohesion members`,
                                  `Social support on the farm`, `Proximity to agriculture`, `Health`, `Quality of life`, 
                                  `Civic engagement`, `Supply stability`, `Food prices`, `Willingness to pay`,
                                  `Time spent on food purchasing`))
  ) %>%
  select(Gruppe, Biodiversity, `Climate Water`, `Soil fertility`,
         `Working conditions`, `Farm economy`,
         `Regional economy`, Society, Members)


#### Gruppenvergleiche 

### Unterschiede in Konstrukten 

# Gruppierungsvariable als Faktor setzen (Reihenfolge definieren)
ds_scales$Gruppe <- factor(ds_scales$Gruppe, levels = c("Converted farms", "Farms interested in conversion","Advisors"))

# Konstrukte, auf die t-Tests angewendet werden sollen
konstrukte <- c("Biodiversity", "Climate Water", "Soil fertility",
                 "Working conditions", "Farm economy",
                 "Regional economy", "Society", "Members")


# T-Tests für alle Konstrukte
ttest_konstrukte <- lapply(konstrukte, function(k) {
  data_subset <- droplevels(ds_scales[which(ds_scales$Gruppe %in% c("Converted farms", "Farms interested in conversion")), ])
  t_test <- t.test(as.formula(paste0("`", k, "` ~ Gruppe")), data = data_subset)
  #### LE: hier nur betriebe unterscheiden
  broom::tidy(t_test) %>%
    mutate(Konstrukt = k) %>%
    select(Konstrukt, estimate1, estimate2, estimate, statistic, p.value, conf.low, conf.high)
}) %>%
  bind_rows() %>%
  rename(
    Mittelwert_umgestellt = estimate1,
    Mittelwert_umstellungsbereit = estimate2,
    Differenz_Mittelwerte = estimate,
    t_Wert = statistic,
    p_Wert = p.value,
    Konfidenz_Unten = conf.low,
    Konfidenz_Oben = conf.high
  ) %>%
  # Auf 2 Nachkommastellen runden
  mutate(across(
    c(Mittelwert_umgestellt, Mittelwert_umstellungsbereit, Differenz_Mittelwerte,
      t_Wert, p_Wert, Konfidenz_Unten, Konfidenz_Oben),
    ~ round(.x, 2)
  ))

# Ergebnis als Objekt im Environment
ttest_konstrukte



### Unterschiede in Items 

# Gruppierungsvariable als Faktor (sicherstellen)
ds_all$Gruppe <- factor(ds_all$Gruppe, levels = c("Converted farms", "Farms interested in conversion","Advisors"))
overallmeans <- colMeans(ds_all[,1:50],na.rm=T)
sum(overallmeans<0)
names(overallmeans)[order(overallmeans)]
overallmeans[order(overallmeans)]

# Alle Items auswählen (alles außer "Gruppe")
items <- setdiff(colnames(ds_all), "Gruppe")

# T-Tests für alle Items
ttest_items <- lapply(items, function(it) {
  formula_it <- as.formula(paste0("`", it, "` ~ Gruppe"))
  t_test <- t.test(formula_it, data = ds_all[which(ds_all$Gruppe%in%c("Converted farms", "Farms interested in conversion")),])
  broom::tidy(t_test) %>%
    mutate(Item = it) %>%
    select(Item, estimate1, estimate2, estimate, statistic, p.value, conf.low, conf.high)
}) %>%
  bind_rows() %>%
  rename(
    Mittelwert_umgestellt = estimate1,
    Mittelwert_umstellungsbereit = estimate2,
    Differenz_Mittelwerte = estimate,
    t_Wert = statistic,
    p_Wert = p.value,
    Konfidenz_Unten = conf.low,
    Konfidenz_Oben = conf.high
  ) %>%
  # Auf 2 Nachkommastellen runden
  mutate(across(
    c(Mittelwert_umgestellt, Mittelwert_umstellungsbereit, Differenz_Mittelwerte,
      t_Wert, p_Wert, Konfidenz_Unten, Konfidenz_Oben),
    ~ round(.x, 3)
  ))

# Ergebnis als Objekt im Environment
ttest_items



####### PLOTS ########

### GRAPHISCHE DARSTELLUNG ITEMS OHNE BERATENDEN 

#Hintergrundfarben definieren
background_df <- data.frame(
  xmin = c(-Inf, -1, 0, 1),
  xmax = c(-1, 0, 1, Inf),
  fill = c("#B3CDE3", "#DDEAF4", "#D5EAD0", "#A8D5BA")
)


#Long Format
ds_all_long <- ds_all %>%
  pivot_longer(
    cols = -c(Gruppe),
    names_to = "Variable",
    values_to = "Wert"
  )

#Zusammenfassung
summary_all <- ds_all_long %>%
  filter(Gruppe %in% c("Converted farms", "Farms interested in conversion")) %>%
  group_by(Gruppe, Variable) %>%
  summarise(
    Mittelwert = mean(Wert, na.rm = TRUE),
    SD = sd(Wert, na.rm = TRUE),
    n = sum(!is.na(Wert)),
    SE = SD / sqrt(n),
    CI_low = Mittelwert - qt(0.975, df = n-1) * SE,
    CI_high = Mittelwert + qt(0.975, df = n-1) * SE
  ) %>%
  ungroup()

#Zuordnung Item → Konstrukt
item_konstrukt <- data.frame(
  Variable = colnames(ds_all)[colnames(ds_all) != "Gruppe"],
  Konstrukt = c(
    rep("Biodiversity",6),
    rep("Climate Water",8),
    rep("Soil fertility",3),
    rep("Working conditins",3),
    rep("Farm economy",7),
    rep("Regional economy",5),
    rep("Society",6),
    rep("Members",12)
  )
)

summary_all <- left_join(summary_all, item_konstrukt, by = "Variable")

####Reihenfolge der Items an die der Konstrukte fixieren 
desired_order <- c("Biodiversity", "Climate Water", "Soil fertility", "Working conditins", "Farm economy", "Regional economy", "Society", "Members")
summary_all$Konstrukt <- factor(summary_all$Konstrukt, levels = desired_order)

#Reihenfolge der Items fixieren
summary_all$Variable <- factor(summary_all$Variable, levels = rev(item_konstrukt$Variable))

# signfiikante items
vecSignificant <- as.vector(ttest_items[which(ttest_items$p_Wert<0.05),"Item"])$Item

# Plot erstellen
Fig1S <- 
  ggplot(summary_all) +
    geom_rect(
      data = background_df,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
      inherit.aes = FALSE,
      alpha = 0.4
    ) +
    scale_fill_identity() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_point(aes(x = Mittelwert, y = Variable, color = Gruppe), size = 1.5,
               position = position_dodge(width = 0.6)) +
    geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, y = Variable, color = Gruppe),
                   height = 0.2, position = position_dodge(width = 0.6)) +
    scale_x_continuous(
      breaks = -2:2,
      labels = c("negative", "rather negative", "neutral", "rather positive", "positive")
    ) +
    scale_color_manual(values = c(
      "Converted farms" = "steelblue",
      "Farms interested in conversion" = "black"
    )) +
    theme_bw() +
    labs(
      x = "Effect",
      y = "",
      color = "Group"
    ) +
    theme(
      text = element_text(size = 8),
      strip.background = element_rect(fill = "grey85", color = NA),
      strip.text = element_text(face = "bold"),
      strip.placement = "outside"
    ) +
    facet_wrap(~ Konstrukt, ncol = 1, scales = "free_y")+
    theme(legend.position="none")+
  geom_text(
    data = subset(summary_all, 
                  Variable %in% vecSignificant & Gruppe == "Farms interested in conversion"),
    aes(x = 2.5, y = Variable, label = "*", color = "black"),
    vjust = 0.75,   # leicht rechts vom Punkt
    size = 5,
    position = position_dodge(width = 0.6)
  )

# jpeg("FigS1.jpeg", width = 16.9, height = 35, units = 'cm', res = 600)
#   FigS1
# dev.off()


########### Graphische Darstellung Konstrukte 

### LONG FORMAT
ds_scales_long <- ds_scales %>%
  pivot_longer(
    cols = -c(Gruppe),
    names_to = "Konstrukt",
    values_to = "Wert"
  )

### Zusammenfassung
summary_scales <- ds_scales_long %>%
  filter(Gruppe %in% c("Converted farms", "Farms interested in conversion")) %>%
  group_by(Gruppe, Konstrukt) %>%
  summarise(
    Mittelwert = mean(Wert, na.rm = TRUE),
    SD = sd(Wert, na.rm = TRUE),
    n = sum(!is.na(Wert)),
    SE = SD / sqrt(n),
    CI_low = Mittelwert - qt(0.975, df = n-1) * SE,
    CI_high = Mittelwert + qt(0.975, df = n-1) * SE
  ) %>%
  ungroup()

# Reihenfolge der Konstrukte fixieren
summary_scales$Konstrukt <- factor(summary_scales$Konstrukt,
                                   levels = rev(colnames(ds_scales)[colnames(ds_scales) != "Gruppe"]))


# signfiikante constructs
vecSignificantConstruct <- as.vector(ttest_konstrukte[which(ttest_konstrukte$p_Wert<0.05),"Konstrukt"])$Konstrukt

### PLOT

Fig2 <- 
  ggplot(summary_scales[which(summary_scales$Konstrukt!="Working conditions"),]) +
    geom_rect(
      data = background_df,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
      inherit.aes = FALSE,
      alpha = 0.4
    ) +
    scale_fill_identity() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    
    geom_point(aes(x = Mittelwert, y = Konstrukt, color = Gruppe), 
               size = 2, position = position_dodge(width = 0.6)) +
    geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, y = Konstrukt, color = Gruppe),
                   height = 0.2, position = position_dodge(width = 0.6)) +
    
    scale_x_continuous(
      limits = c(-2, 2),
      breaks = -2:2,
      labels = c("negative", "rather negative", "neutral", "rather positive", "positive")
    ) +
    scale_color_manual(values = c(
      "Converted farms" = "steelblue",
      "Farms interested in conversion" = "black"
    )) +
    
    theme_bw() +
    labs(
      x = "Effect",
      y = "",
      color = "Group"
    ) +
    theme(
      text = element_text(size = 10)
    )+
  theme(legend.position="none")+
    geom_text(
      data = subset(summary_scales, 
                    Konstrukt%in% vecSignificantConstruct & Gruppe == "Farms interested in conversion"),
      aes(x = 2, y = Konstrukt, label = "*", color = "black"),
      vjust = 1,   # leicht rechts vom Punkt
      size = 10,
      position = position_dodge(width = 0.6)
    )


# jpeg("Fig2.jpeg", width = 18, height = 12, units = "cm", res = 600)
#   Fig2
# dev.off()




rm(list=ls())




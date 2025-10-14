###### 
## written by Lukas Egli, Hanna Haf
## 13 October 2025

###### libraries
library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(Hmisc)

###### backgroupnd color for plot
background_df <- data.frame(
  xmin = c(-Inf, -1, 0, 1),
  xmax = c(-1, 0, 1, Inf),
  fill = c("#B3CDE3", "#DDEAF4", "#D5EAD0", "#A8D5BA")
)

###### load and prepare data

### dataset 1: farms interested in conversion
dfUmfrage1 <- read.xlsx("data/20251013_CSASustainability_dataset1.xlsx",sheetIndex=1)
head(dfUmfrage1)
names(dfUmfrage1)
dfUmfrage1 <- dfUmfrage1[,c(1,5:55)]
names(dfUmfrage1) <- c("Datum","Name",
                      "Nutzpflanzenvielfalt","Lebensräume","Pflanzenschutz","Nutztiervielfalt","Grünlandbewirtschaftung","Tierwohl",
                      "Energieverbrauch","Kraftstoffverbrauch","Erneuerbare Energien","Materialverbrauch","Transport und Verkehr","Wasserverbrauch","Landnutzung und Tierhaltung","Lebensmittelverluste","Düngung","Bodenbearbeitung","Fruchtfolge",
                      "Lohn und Gewinn","Arbeitsplatzqualität","Einbindung der Angestellten","Anzahl Betriebszweige","Betriebseigentum","Verlässlicher Umsatz","Liquidität","Subventionsabhängigkeit","Entlohnung von Ökosystemleistungen","Geschlossener Hofkreislauf","Regionale Vermarktung","Regionaler Zukauf","Arbeitsplätze","Betriebliche Kooperationen","Soziale Kooperationen",
                      "Pädagogische Angebote","Vermittlung von Fachwissen","Inklusion und Integration","Transparenz","Partizipation","Erhalt von Kulturgut",
                      "Umweltbewusstsein","Nachhaltiges Verhalten","Sozialer Zusammenhalt","Soziale Unterstützung","Nähe zur Landwirtschaft","Gesundheit","Lebensqualität","zivilgesellschaftliches Engagement","Versorgungsstabilität","Lebensmittelkosten","Zahlungsbereitschaft","Zeitaufwand Lebensmittelbeschaffung")


## add information for each farm based on qualitative research
dfUmfrage1$Name
dfUmfrage1$Bundesland <- c("Niedersachsen","Niedersachsen","NRW","NRW","Sachsen","Thüringen")
dfUmfrage1$WirtschaftlicheAusrichtung <- c("Ackerbaubetrieb",
                                                  "Pflanzenbauverbundbetrieb",
                                                  "Viehhaltungsverbundbetrieb",
                                                  "Viehhaltungsverbundbetrieb",
                                                  "Ackerbaubetrieb",
                                                  "Futterbaubetrieb")
dfUmfrage1$Vermarktungsform <-  "Beide Vermarktungswege"
dfUmfrage1$Flaeche <- c(7.5,130,115,70,200,11)
dfUmfrage1$Ernteanteile <- c(50,200,NA,NA,NA,80)
dfUmfrage1$Jahr <- 2025
dfUmfrage1$AenderungWirtschaftsweise <- c("Nein","Nein","Nein","Ja","Nein","Nein")
dfUmfrage1$AenderungBetriebszweige  <- c("Ja","Ja","Ja","Ja","Ja","Ja")
dfUmfrage1$AnteilUmstellung  <- c(100,100,NA,NA,NA,NA)
dfUmfrage1$AenderungKooperationen  <- c("Ja","Nein","Ja","Ja","Ja","Ja")
dfUmfrage1$AenderungArbeitsplätze  <- c("Ja","Nein","Ja",NA,"Ja","Ja")
dfUmfrage1$Typ  <- c("Erzeuger","Erzeuger","Erzeuger","Erzeuger","Erzeuger","Erzeuger")
dfUmfrage1$Naehe  <- c("Ja","Ja","Ja","Ja","Ja","Nein")
dfUmfrage1$Transport  <- c("Nein","Nein","Ja","Ja","Ja","Ja")
dfUmfrage1$Beitragsgestaltung <-  c(NA,"Nein","Ja","Nein","Nein",NA)
dfUmfrage1$Mitbestimmung  <- c("Ja","Nein","Ja","Ja",NA,NA)
dfUmfrage1$Mitarbeit  <- c(NA,"Ja","Ja","Ja",NA,NA)
names(dfUmfrage1)
dfUmfrage1 <- dfUmfrage1[,c(2,53:69,3:52)]
nrow(dfUmfrage1)

# change NA to 0 (neutral effect)
dfUmfrage1[,19:68]
dfUmfrage1[which(is.na(dfUmfrage1$`Erneuerbare Energien`)),"Erneuerbare Energien"] <- 0


### dataset 2: farms already converted
dfUmfrage2 <- read.xlsx("data/20251013_CSASustainability_dataset2.xlsx",sheetIndex=1,startRow=1)
head(dfUmfrage2)
names(dfUmfrage2)
dfUmfrage2 <- dfUmfrage2[,c(2,1,12,7,10,9,101:103,105,100,
                            11,13,14,32,34,31,42, 40,15:17,19:30,50:99)]
names(dfUmfrage2)
names(dfUmfrage2) <- c("Datum","Name","Bundesland","WirtschaftlicheAusrichtung","Vermarktungsform","Wirtschaftsweise","Weiblich","Männlich","Divers","Bildung","Alter",
                       "Flaeche","Ernteanteile","Jahr",
                       "AenderungWirtschaftsweise","AenderungBetriebszweige","AnteilUmstellung","AenderungKooperationen","AenderungArbeitsplätze","Typ1","Typ2","Typ3",
                       "LieferungNachHause","Depots","Selbstabholung","Selbsternte","LogistikSonstiges",
                       "Festbetrag","FestbetragSolidartopf","Gestaffelt","Beitragsrunde","BeitragsgestaltungSonstiges",
                       "Mitbestimmung","Mitarbeit",
                       "Nutzpflanzenvielfalt","Lebensräume","Pflanzenschutz","Nutztiervielfalt","Grünlandbewirtschaftung","Tierwohl",
                       "Energieverbrauch","Kraftstoffverbrauch","Erneuerbare Energien","Materialverbrauch","Transport und Verkehr","Wasserverbrauch","Landnutzung und Tierhaltung","Lebensmittelverluste",
                       "Düngung","Bodenbearbeitung","Fruchtfolge",
                       "Lohn und Gewinn","Arbeitsplatzqualität","Einbindung der Angestellten",
                       "Anzahl Betriebszweige","Betriebseigentum","Verlässlicher Umsatz","Liquidität","Subventionsabhängigkeit","Entlohnung von Ökosystemleistungen","Geschlossener Hofkreislauf",
                       "Regionale Vermarktung","Regionaler Zukauf","Arbeitsplätze","Betriebliche Kooperationen","Soziale Kooperationen",
                       "Pädagogische Angebote","Vermittlung von Fachwissen","Inklusion und Integration","Transparenz","Partizipation","Erhalt von Kulturgut",
                       "Umweltbewusstsein","Nachhaltiges Verhalten","Sozialer Zusammenhalt","Soziale Unterstützung","Nähe zur Landwirtschaft","Gesundheit","Lebensqualität","zivilgesellschaftliches Engagement","Versorgungsstabilität","Lebensmittelkosten","Zahlungsbereitschaft","Zeitaufwand Lebensmittelbeschaffung")

## remove lines with mostly NAs (non completed surveys)
names(dfUmfrage2)
dfUmfrage2$isNa <- apply(dfUmfrage2[3:84],1,function(i)sum(is.na(i)))
dfUmfrage2 <- dfUmfrage2[which(dfUmfrage2$isNa<50),1:84]
nrow(dfUmfrage2)

## harmonize state names
dfUmfrage2$Bundesland
dfUmfrage2[which(dfUmfrage2$Bundesland%in%c("Baden Württemberg","Baden Württemberg ","Baden-Württemberg ")),"Bundesland"] <- "Baden-Württemberg"
dfUmfrage2[which(dfUmfrage2$Bundesland%in%c("nrw","NRW")),"Bundesland"] <- "NRW"
dfUmfrage2[which(dfUmfrage2$Bundesland%in%c("Schleswig Holstein")),"Bundesland"] <- "Schleswig-Holstein"
dfUmfrage2[which(dfUmfrage2$Bundesland%in%c("MV")),"Bundesland"] <- "Mecklenburg-Vorpommern"
table(dfUmfrage2$Bundesland)

## create one column for each aspect
dfUmfrage2[,c("Weiblich","Männlich","Divers")]
dfUmfrage2$Geschlecht <- NA
dfUmfrage2[which(dfUmfrage2$Weiblich=="Ja"),"Geschlecht"] <- "Weiblich"
dfUmfrage2[which(dfUmfrage2$Männlich=="Ja"),"Geschlecht"] <- "Männlich"

dfUmfrage2[,c("Typ1","Typ2","Typ3")]
dfUmfrage2$Typ <- "Erzeuger"
dfUmfrage2[which(dfUmfrage2$Typ2=="Ja"),"Typ"] <- "Kooperation"
dfUmfrage2[which(dfUmfrage2$Typ3=="Ja"),"Typ"] <- "Mitunternehmer"

dfUmfrage2[,c("LieferungNachHause","Depots","Selbstabholung","Selbsternte","LogistikSonstiges")]
dfUmfrage2$Naehe <- "Nein"
dfUmfrage2[which(dfUmfrage2$Selbstabholung=="Ja"|dfUmfrage2$Selbsternte=="Ja"),"Naehe"] <- "Ja"
dfUmfrage2$Transport <- "Nein"
dfUmfrage2[which(dfUmfrage2$LieferungNachHause=="Ja"|dfUmfrage2$Depots=="Ja"),"Transport"] <- "Ja"

dfUmfrage2[,c("Festbetrag","FestbetragSolidartopf","Gestaffelt","Beitragsrunde","BeitragsgestaltungSonstiges")]
dfUmfrage2$Beitragsgestaltung <- NA
dfUmfrage2[which(dfUmfrage2$Festbetrag =="Ja"),"Beitragsgestaltung"] <- "Festbetrag"
dfUmfrage2[which(dfUmfrage2$FestbetragSolidartopf =="Ja"),"Beitragsgestaltung"] <- "FestbetragSolidartopf"
dfUmfrage2[which(dfUmfrage2$Gestaffelt =="Ja"),"Beitragsgestaltung"] <- "Gestaffelt"
dfUmfrage2[which(dfUmfrage2$Beitragsrunde =="Ja"),"Beitragsgestaltung"] <- "Beitragsrunde"
table(dfUmfrage2$Beitragsgestaltung)
dfUmfrage2[which(dfUmfrage2$Beitragsgestaltung=="Festbetrag"),"Beitragsgestaltung"] <- "Nein"
dfUmfrage2[which(dfUmfrage2$Beitragsgestaltung%in%c("FestbetragSolidartopf","Gestaffelt","Beitragsrunde")),"Beitragsgestaltung"] <- "Ja"

unique(dfUmfrage2$WirtschaftlicheAusrichtung)
dfUmfrage2[which(dfUmfrage2$WirtschaftlicheAusrichtung=="Dauerkulturbetrieb (>2/3 Umsatz mit Obst, Beerenobstanlage, Rebanlagen, sonstige Dauerkulturen etc.)" ),"WirtschaftlicheAusrichtung"] <- "Dauerkulturbetrieb"
dfUmfrage2[which(dfUmfrage2$WirtschaftlicheAusrichtung=="Gartenbaubetrieb (>2/3 Umsatz mit Gemüse, Blumen, Zierpflanzen, Baumschule etc.)"),"WirtschaftlicheAusrichtung"] <- "Gartenbaubetrieb"
dfUmfrage2[which(dfUmfrage2$WirtschaftlicheAusrichtung=="Ackerbaubetrieb (>2/3 Umsatz mit Ackerbau Getreide, Kartoffeln, Zuckerrüben, Futterpflanzen, Grünbrache etc.)" ),"WirtschaftlicheAusrichtung"] <- "Ackerbaubetrieb"
dfUmfrage2[which(dfUmfrage2$WirtschaftlicheAusrichtung=="Viehhaltungsverbundbetrieb (primär Grünland und Weidevieh/Veredelung MIT Acker-/Gartenbau/Dauerkulturren)" ),"WirtschaftlicheAusrichtung"] <- "Viehhaltungsverbundbetrieb"
dfUmfrage2[which(dfUmfrage2$WirtschaftlicheAusrichtung=="Pflanzenbau-Viehhaltungsverbundbetrieb (Betriebe die von Klasse 1-7 ausgeschlossen wurden)" ),"WirtschaftlicheAusrichtung"] <- "PflanzenbauViehhaltungsverbundbetrieb"
dfUmfrage2[which(dfUmfrage2$WirtschaftlicheAusrichtung=="Sonstiges"),"WirtschaftlicheAusrichtung"] <- NA

names(dfUmfrage2)
dfUmfrage2 <- dfUmfrage2[,c(2:6,10:19,23:27,33:34,85:89,35:84)]
names(dfUmfrage2)

## create farm size classes
dfUmfrage2[which(dfUmfrage2$Flaeche=="32 ha"),"Flaeche"] <- 32 # to numeric
dfUmfrage2$Flaeche <- as.numeric(gsub(",", ".", dfUmfrage2$Flaeche))

dfUmfrage2$FlaecheKlasse <- "bis5"
dfUmfrage2[which(dfUmfrage2$Flaeche>5),"FlaecheKlasse"] <- "5-10"
dfUmfrage2[which(dfUmfrage2$Flaeche>10),"FlaecheKlasse"] <- "10-20"
dfUmfrage2[which(dfUmfrage2$Flaeche>20),"FlaecheKlasse"] <- "20-50"
dfUmfrage2[which(dfUmfrage2$Flaeche>50),"FlaecheKlasse"] <- "50-100"
dfUmfrage2[which(dfUmfrage2$Flaeche>100),"FlaecheKlasse"] <- "100-200"
dfUmfrage2[which(dfUmfrage2$Flaeche>200),"FlaecheKlasse"] <- "200-500"
dfUmfrage2[which(dfUmfrage2$Flaeche>500),"FlaecheKlasse"] <- "500-1000"
dfUmfrage2[which(dfUmfrage2$Flaeche>1000),"FlaecheKlasse"] <- "mehr1000"

dfUmfrage2[which(dfUmfrage2$Ernteanteile=="ca. 50"),"Ernteanteile"] <- "50"
dfUmfrage2$Ernteanteile <- as.numeric(dfUmfrage2$Ernteanteile)

dfUmfrage2$Alter
dfUmfrage2$AlterKlasse <- NA
dfUmfrage2[which(dfUmfrage2$Alter>0),"AlterKlasse"] <- "bis30"
dfUmfrage2[which(dfUmfrage2$Alter>30),"AlterKlasse"] <- "30-40"
dfUmfrage2[which(dfUmfrage2$Alter>40),"AlterKlasse"] <- "40-50"
dfUmfrage2[which(dfUmfrage2$Alter>50),"AlterKlasse"] <- "mehr50"

dfUmfrage2$AnteilUmstellung
dfUmfrage2$Umstellung <- NA
dfUmfrage2[which(dfUmfrage2$AnteilUmstellung>0),"Umstellung"] <- "teilweise"
dfUmfrage2[which(dfUmfrage2$AnteilUmstellung==100),"Umstellung"] <- "vollständig"

dfUmfrage2[which(dfUmfrage2$AenderungWirtschaftsweise%in%c("Sonstiges","Nein (Anbauweise blieb bestehen)")),"AenderungWirtschaftsweise"] <- "Nein (Anbauweise blieb bestehen)"

lapply(dfUmfrage2[,c("Bundesland","WirtschaftlicheAusrichtung","FlaecheKlasse","Vermarktungsform","Wirtschaftsweise",
                     "AlterKlasse","Geschlecht","Bildung","Beitragsgestaltung",
                     "Umstellung","AenderungWirtschaftsweise","AenderungBetriebszweige","AenderungArbeitsplätze","AenderungKooperationen","Typ",
                     "LieferungNachHause","Depots","Selbstabholung","Selbsternte","Mitbestimmung","Mitarbeit")] , function(x) table(x))

names(dfUmfrage2)
dfUmfrage2[which(dfUmfrage2$Vermarktungsform=="Indirekte Vermarktung (Lieferverträge mit Einzelhandel und Großkunden)"),"Vermarktungsform"] <- "Beide Vermarktungswege" # korrektur weil Fehler

dfUmfrage2 <- dfUmfrage2[names(dfUmfrage1)]

names(dfUmfrage2)
dfUmfrage2 <- dfUmfrage2 %>%
  mutate(across(19:68, ~ recode(.,
                                "negative Effekte" = -2,
                                "eher negative Effekte" = -1,
                                "neutrale Effekte" = 0,
                                "eher positive Effekte" = 1,
                                "positive Effekte" = 2,
                                "nicht relevant" = 0 # not relevant to neutral
  )))


## merge both datasets
dfAll <- rbind(dfUmfrage2,dfUmfrage1)
names(dfAll)
nrow(dfAll)


## constructs
biodiversitaet <- c("Nutzpflanzenvielfalt","Lebensräume","Pflanzenschutz","Nutztiervielfalt","Grünlandbewirtschaftung","Tierwohl")
dfAll[biodiversitaet]
psych::alpha(dfAll[biodiversitaet], check.keys=TRUE) # 1.1 Cronbachs alpha

klimawasser <- c("Energieverbrauch","Kraftstoffverbrauch","Erneuerbare Energien","Materialverbrauch","Transport und Verkehr","Wasserverbrauch","Landnutzung und Tierhaltung","Lebensmittelverluste")
dfAll[klimawasser]
psych::alpha(dfAll[klimawasser], check.keys=TRUE)

bodenfruchtbarkeit <- c("Düngung","Bodenbearbeitung","Fruchtfolge")
dfAll[bodenfruchtbarkeit]
psych::alpha(dfAll[bodenfruchtbarkeit], check.keys=TRUE)

arbeitsbedingungen <- c("Lohn und Gewinn","Arbeitsplatzqualität","Einbindung der Angestellten")
dfAll[arbeitsbedingungen]
psych::alpha(dfAll[arbeitsbedingungen], check.keys=TRUE) # low quality

wirtschaft <- c("Anzahl Betriebszweige","Betriebseigentum","Verlässlicher Umsatz","Liquidität","Subventionsabhängigkeit","Entlohnung von Ökosystemleistungen","Geschlossener Hofkreislauf")
dfAll[wirtschaft]
psych::alpha(dfAll[wirtschaft], check.keys=TRUE)
wirtschaft <- c("Anzahl Betriebszweige","Betriebseigentum","Verlässlicher Umsatz","Liquidität","Entlohnung von Ökosystemleistungen","Geschlossener Hofkreislauf") # remove depencence on subsides to improve raw alpha
psych::alpha(dfAll[wirtschaft], check.keys=TRUE)

vernetzung <- c("Regionale Vermarktung","Regionaler Zukauf","Arbeitsplätze","Betriebliche Kooperationen","Soziale Kooperationen")
dfAll[vernetzung]
psych::alpha(dfAll[vernetzung], check.keys=TRUE)

gesellschaft <- c("Pädagogische Angebote","Vermittlung von Fachwissen","Inklusion und Integration","Transparenz","Partizipation","Erhalt von Kulturgut")
dfAll[gesellschaft]
psych::alpha(dfAll[gesellschaft], check.keys=TRUE)

mitglieder <- c("Umweltbewusstsein","Nachhaltiges Verhalten","Sozialer Zusammenhalt","Soziale Unterstützung","Nähe zur Landwirtschaft","Gesundheit","Lebensqualität","zivilgesellschaftliches Engagement","Versorgungsstabilität","Lebensmittelkosten","Zahlungsbereitschaft","Zeitaufwand Lebensmittelbeschaffung")
dfAll[mitglieder]
psych::alpha(dfAll[mitglieder], check.keys=TRUE)

## create constructs
dfAll$Biodiversitaet <- rowMeans(dfAll[biodiversitaet],na.rm=T)
dfAll$KlimaWasser <- rowMeans(dfAll[klimawasser],na.rm=T)
dfAll$Bodenfruchtbarkeit <- rowMeans(dfAll[bodenfruchtbarkeit],na.rm=T)
dfAll$Arbeitsbedinungen <- rowMeans(dfAll[arbeitsbedingungen],na.rm=T)
dfAll$Wirtschaft <- rowMeans(dfAll[wirtschaft],na.rm=T)
dfAll$Vernetzung <- rowMeans(dfAll[vernetzung],na.rm=T)
dfAll$Gesellschaft <- rowMeans(dfAll[gesellschaft],na.rm=T)
dfAll$Mitglieder <- rowMeans(dfAll[mitglieder],na.rm=T)

## change to factors
names(dfAll)
dfAll[which(dfAll$AenderungWirtschaftsweise%in%c("Sonstiges","Nein (Anbauweise blieb bestehen)")),"AenderungWirtschaftsweise"] <- "Nein"
dfAll[which(dfAll$AenderungWirtschaftsweise=="Ja (von konventionell auf ökologisch)"),"AenderungWirtschaftsweise"] <- "Ja"
dfAll$AenderungWirtschaftsweise <- factor(dfAll$AenderungWirtschaftsweise,levels=c("Nein","Ja"))
dfAll$AenderungBetriebszweige <- factor(dfAll$AenderungBetriebszweige,levels=c("Nein","Ja"))
dfAll$AenderungArbeitsplätze <- factor(dfAll$AenderungArbeitsplätze,levels=c("Nein","Ja"))
dfAll$AenderungKooperationen <- factor(dfAll$AenderungKooperationen,levels=c("Nein","Ja"))
dfAll$AenderungNaehe <- factor(dfAll$Naehe,levels=c("Nein","Ja"))
dfAll$AenderungTransport <- factor(dfAll$Transport,levels=c("Nein","Ja"))
dfAll$Beitragsgestaltung <- factor(dfAll$Beitragsgestaltung,levels=c("Nein","Ja"))
dfAll$AenderungMitbestimmung <- "Nein" 
dfAll[which(dfAll$Mitbestimmung!="gering"),"AenderungMitbestimmung"] <- "Ja"
dfAll$AenderungMitbestimmung <- factor(dfAll$AenderungMitbestimmung,levels=c("Nein","Ja"))
dfAll$AenderungMitarbeit <- "Nein" 
dfAll[which(dfAll$Mitarbeit!="gering"),"AenderungMitarbeit"] <- "Ja"
dfAll$AenderungMitarbeit <- factor(dfAll$AenderungMitarbeit,levels=c("Nein","Ja"))
dfAll$AenderungVermarktungsform <- "Nein"
dfAll[which(dfAll$Vermarktungsform=="Direktvermarktung (eigene Vermarktung)"),"AenderungVermarktungsform"] <- "Ja"
dfAll$AenderungVermarktungsform <- factor(dfAll$AenderungVermarktungsform,levels=c("Nein","Ja"))
names(dfAll)
dfFinal  <- dfAll[,c(1:18,69:81)]



###### Analyses
### Analysis 1: Sustainability aspects

## Construct level
dfFinalGroup <- dfFinal[,c(1,19:26)]
dfFinalGroup$Group <- "ConvertedFarms"
dfFinalGroup[which(dfFinalGroup$Name%in%c("AV","HW","HH","Hhu","LF","BK" )),"Group"] <- "InterestedFarms"

# constructs
konstrukte <- c("Biodiversity", "Climate Water", "Soil fertility",
                "Working conditions", "Farm economy",
                "Regional economy", "Society", "Members")

names(dfFinalGroup)[2:9] <- konstrukte

# T-Tests for all constructs
ttest_konstrukte <- lapply(konstrukte, function(k) {
  t_test <- t.test(as.formula(paste0("`", k, "` ~ Group")), data = dfFinalGroup)
  broom::tidy(t_test) %>%
    mutate(Konstrukt = k) %>%
    select(Konstrukt, estimate1, estimate2, estimate, statistic, p.value, conf.low, conf.high)
}) %>%
  bind_rows() %>%
  # Auf 2 Nachkommastellen runden
  mutate(across(
    c(estimate1, estimate2, estimate,
      statistic, p.value, conf.low, conf.high),
    ~ round(.x, 2)
  ))
ttest_konstrukte

# Long format
names(dfFinalGroup)
ds_scales_long <- dfFinalGroup[,2:10] %>%
  pivot_longer(
    cols = -c(Group),
    names_to = "Konstrukt",
    values_to = "Wert"
  )

# summary
summary_scales <- ds_scales_long %>%
  group_by(Group, Konstrukt) %>%
  summarise(
    Mittelwert = mean(Wert, na.rm = TRUE),
    SD = sd(Wert, na.rm = TRUE),
    n = sum(!is.na(Wert)),
    SE = SD / sqrt(n),
    CI_low = Mittelwert - qt(0.975, df = n-1) * SE,
    CI_high = Mittelwert + qt(0.975, df = n-1) * SE
  ) %>%
  ungroup()

# Fix order
summary_scales$Konstrukt <- factor(summary_scales$Konstrukt,
                                   levels = rev(names(dfFinalGroup)[1:9]))


# significant constructs
vecSignificantConstruct <- as.vector(ttest_konstrukte[which(ttest_konstrukte$p.value<0.05),"Konstrukt"])$Konstrukt

#plot
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
  
  geom_point(aes(x = Mittelwert, y = Konstrukt, color = Group), 
             size = 2, position = position_dodge(width = 0.6)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, y = Konstrukt, color = Group),
                 height = 0.2, position = position_dodge(width = 0.6)) +
  
  scale_x_continuous(
    limits = c(-2, 2),
    breaks = -2:2,
    labels = c("negative", "rather negative", "neutral", "rather positive", "positive")
  ) +
  scale_color_manual(values = c(
    "ConvertedFarms" = "steelblue",
    "InterestedFarms" = "black"
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
                  Konstrukt%in% vecSignificantConstruct & Group == "InterestedFarms"),
    aes(x = 2, y = Konstrukt, label = "*", color = "black"),
    vjust = 1,   # leicht rechts vom Punkt
    size = 10,
    position = position_dodge(width = 0.6)
  )


## Item level
dfFinalGroupItem <- dfAll[,c(1,19:68)]
dfFinalGroupItem$Group <- "ConvertedFarms"
dfFinalGroupItem[which(dfFinalGroupItem$Name%in%c("AV","HW","HH","Hhu","LF","BK" )),"Group"] <- "InterestedFarms"

items <- c("Crop diversity", "Creation of habitats", "Form of plant protection", "Livestock diversity", "Grassland management", "Animal welfare", 
  "Energy consumption", "Fuel consumption", "Use of renewable energies", "Material consumption", "Transport and traffic", "Water usage", "Land use and animal husbandry", "Food losses", "Form of fertilization", "Form of soil cultivation", "Crop rotation",
  "Wages and profits", "Workplace quality", "Involvement of employees", "Number of business branches", "Proportion of land owned by the farm", "Reliable turnover", "Liquidity for reserves or equity capital", "Dependence on subsidies", "Remuneration for ecosystem services provided", "Closed farm cycle", 
  "Increase regional marketing", "Regional purchases of production resources", "Creation of regional jobs", "Expansion of business cooperation", "Expansion of social cooperation",
  "Educational programs", "Transfer of specialist knowledge", "Inclusion and integration", "Transparency", "Participation", "Preservation of cultural heritage",
  "Environmental consciousness", "Sustainability behavior", "Social cohesion members", "Social support on the farm", "Proximity to agriculture", "Health", "Quality of life", "Civic engagement", "Supply stability", "Food prices", "Willingness to pay", "Time spent on food purchasing")

names(dfFinalGroupItem)[2:51] <- items

# T-Tests for all items
ttest_items <- lapply(items, function(it) {
  formula_it <- as.formula(paste0("`", it, "` ~ Group"))
  t_test <- t.test(formula_it, data = dfFinalGroupItem)
  broom::tidy(t_test) %>%
    mutate(Item = it) %>%
    select(Item, estimate1, estimate2, estimate, statistic, p.value, conf.low, conf.high)
}) %>%
  bind_rows() %>%
  mutate(across(
    c(estimate1, estimate2, estimate,
      statistic, p.value, conf.low, conf.high),
    ~ round(.x, 3)
  ))
ttest_items[order(ttest_items$p.value),]

# Long Format
ds_all_long <- dfFinalGroupItem %>%
  pivot_longer(
    cols = -c(Name,Group),
    names_to = "Variable",
    values_to = "Wert"
  )

# Summary
summary_all <- ds_all_long %>%
  group_by(Group, Variable) %>%
  summarise(
    Mittelwert = mean(Wert, na.rm = TRUE),
    SD = sd(Wert, na.rm = TRUE),
    n = sum(!is.na(Wert)),
    SE = SD / sqrt(n),
    CI_low = Mittelwert - qt(0.975, df = n-1) * SE,
    CI_high = Mittelwert + qt(0.975, df = n-1) * SE
  ) %>%
  ungroup()

# add constructs
item_konstrukt <- data.frame(
  Variable = colnames(dfFinalGroupItem)[2:51],
  Konstrukt = c(
    rep("Biodiversity",6),
    rep("Climate Water",8),
    rep("Soil fertility",3),
    rep("Working conditions",3),
    rep("Farm economy",7),
    rep("Regional economy",5),
    rep("Society",6),
    rep("Members",12)
  )
)

summary_all <- left_join(summary_all, item_konstrukt, by = "Variable")

# fix order of constructs and items
desired_order <- c("Biodiversity", "Climate Water", "Soil fertility", "Working conditions", "Farm economy", "Regional economy", "Society", "Members")
summary_all$Konstrukt <- factor(summary_all$Konstrukt, levels = desired_order)
summary_all$Variable <- factor(summary_all$Variable, levels = rev(item_konstrukt$Variable))

# significant items
vecSignificant <- as.vector(ttest_items[which(ttest_items$p.value<0.05),"Item"])$Item

# create plot
FigS1 <- 
  ggplot(summary_all) +
  geom_rect(
    data = background_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.4
  ) +
  scale_fill_identity() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(x = Mittelwert, y = Variable, color = Group), size = 1.5,
             position = position_dodge(width = 0.6)) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high, y = Variable, color = Group),
                 height = 0.2, position = position_dodge(width = 0.6)) +
  scale_x_continuous(
    breaks = -2:2,
    labels = c("negative", "rather negative", "neutral", "rather positive", "positive")
  ) +
  scale_color_manual(values = c(
    "ConvertedFarms" = "steelblue",
    "InterestedFarms" = "black"
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
                  Variable %in% vecSignificant & Group == "InterestedFarms"),
    aes(x = 2.5, y = Variable, label = "*", color = "black"),
    vjust = 0.75,   # leicht rechts vom Punkt
    size = 5,
    position = position_dodge(width = 0.6)
  )




### Analysis 2: Farm acharacteristics and conversion decisions
## change year of foundation to CSA age
dfFinal$Jahr <- 2025-dfFinal$Jahr 
names(dfFinal)
shapiro.test(dfFinal$Ernteanteile)
shapiro.test(dfFinal$Jahr)
shapiro.test(dfFinal$AnteilUmstellung)

## correlation between farm size, CSA age and sustainability constructs
res <- rcorr(as.matrix(dfFinal[, c(5,6,7,10,19:21,23:26)]), type = "spearman")
corTable <- round(res$r[1:4,5:11],2)
round(res$P[1:4,5:11],2)

## group differences regarding conversion decisions for different sustainability constructs
functionSustainability <- function(aspect,dfTarget,responses){
  lapply(responses, function(k) {
    data_subset <- droplevels(dfTarget[which(dfTarget$Gruppe %in% c("Ja", "Nein")), ])
    t_test <- t.test(as.formula(paste0("`", k, "` ~ ",aspect)), data = dfTarget)
    broom::tidy(t_test) %>%
      mutate(Konstrukt = k) %>%
      select(Konstrukt, estimate1, estimate2, estimate, statistic, p.value, conf.low, conf.high)
  }) %>%
    bind_rows()
}

responses1 <- c("Biodiversitaet","KlimaWasser","Bodenfruchtbarkeit","Wirtschaft","Vernetzung","Gesellschaft","Mitglieder")

tableS1a <- functionSustainability("AenderungWirtschaftsweise",df,responses1)
functionSustainability("AenderungWirtschaftsweise",dfAll,klimawasser)

tableS1b <-functionSustainability("AenderungBetriebszweige",df,responses1)
tableS2a <- functionSustainability("AenderungBetriebszweige",dfAll,klimawasser) 

tableS1c <- functionSustainability("AenderungTransport",df,responses1)
functionSustainability("AenderungTransport",dfAll,vernetzung)

tableS1d <- functionSustainability("AenderungKooperationen",df,responses1)

tableS1e <- functionSustainability("AenderungArbeitsplätze",df,responses1)

tableS1f <- functionSustainability("Beitragsgestaltung",df,responses1)

tableS1g <- functionSustainability("AenderungMitarbeit",df,responses1)
functionSustainability("AenderungMitarbeit",dfAll,mitglieder) # kosten
tableS2b <- functionSustainability("AenderungMitarbeit",dfAll,mitglieder) # 

tableS1h <- functionSustainability("AenderungMitbestimmung",df,responses1)

## create figures for significant group differences
fig3a <- ggplot(na.omit(df[,c("AenderungBetriebszweige","KlimaWasser")]), aes(x = AenderungBetriebszweige, y = KlimaWasser , fill = AenderungBetriebszweige)) +
  geom_boxplot(
    outlier.shape = 21,       # Ausreißer als Punkte
    outlier.size = 2,
    width = 0.6
  ) +
  scale_fill_manual(values=c("#f1a340","#542788")) +  # harmonische Farben+
  scale_y_continuous(breaks = -2:2,limits = c(-2,2))+
  scale_x_discrete(labels = c(
    "Nein" = "No new branches",
    "Ja" = "New branches"
  )) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",           # keine Legende nötig
    axis.text = element_text(color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major.x = element_blank() # saubere vertikale Linien
  ) +
  ylab("Climate & water")

fig3b <- ggplot(na.omit(df[,c("AenderungMitarbeit","Mitglieder")]), aes(x = AenderungMitarbeit, y = Mitglieder , fill = AenderungMitarbeit)) +
  geom_boxplot(
    outlier.shape = 21,       # Ausreißer als Punkte
    outlier.size = 2,
    width = 0.6
  ) +
  scale_fill_manual(values=c("#f1a340","#542788")) +  # harmonische Farben+
  scale_y_continuous(breaks = -2:2,limits = c(-2,2))+
  scale_x_discrete(labels = c(
    "Nein" = "Low participation",
    "Ja" = "Medium/high participation"
  )) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "none",           # keine Legende nötig
    axis.text = element_text(color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major.x = element_blank() # saubere vertikale Linien
  ) +
  ylab("Members")



#####  results

# jpeg("Fig2.jpeg", width = 18, height = 12, units = "cm", res = 600)
#   Fig2
# dev.off()

# jpeg("FigS1.jpeg", width = 16.9, height = 35, units = 'cm', res = 600)
#   FigS1
# dev.off()


Table3 <- corTable 
# write.xlsx(Table3,"Table3.xlsx")

tableS1 <- rbind(tableS1a,tableS1b,tableS1c,tableS1d,tableS1e,tableS1f,tableS1g,tableS1h)
tableS1[2:8] <- round(tableS1[2:8],2)
tableS1$estimate <- paste0(tableS1$estimate," (",tableS1$conf.low,"-",tableS1$conf.high,")")
tableS1 <- tableS1[,c("Konstrukt","estimate1","estimate2","estimate","statistic","p.value")]
# write.xlsx(tableS1,"TableS1.xlsx")

tableS2 <- rbind(tableS2a,tableS2b)
tableS2[2:8] <- round(tableS2[2:8],2)
tableS2$estimate <- paste0(tableS2$estimate," (",tableS2$conf.low,"-",tableS2$conf.high,")")
tableS2 <- tableS2[,c("Konstrukt","estimate1","estimate2","estimate","statistic","p.value")]
# write.xlsx(tableS2,"TableS2.xlsx")

Fig3 <- plot_grid(plotlist = c(fig3a,fig3b),labels = c("a","b"),label_size=10,nrow = 1,ncol = 2,align="h")

# jpeg("Fig3.jpeg",width = 16.9, height = 8,units = 'cm', res = 600)
#   Fig3
# dev.off()



rm(list=ls())

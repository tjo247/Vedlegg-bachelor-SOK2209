# Les inn dataene som allerede beskrevet
library(readr)
library(tidyverse)
library(readxl)
library(dplyr)
library(gt)
library(stargazer)
library(jtools)
library(vtable)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

Sys.setlocale(locale="no_NO")

# Last inn Gini_data og ESS_data
Gini_data <- read_excel("gini_wdi.xlsx")
ESS_data <-  read_csv("ESS_trust.csv")
ESS_data <- subset(ESS_data, select = -c(prob, stratum, psu, idno, edition, dweight, anweight, pweight, pspwght))

# Omdøpe variabler
ESS_data <- rename(ESS_data, Tillit = ppltrst, inntektA = hinctnt, inntektB = hinctnta, 
                   gift = marital, gift1 = maritala, gift2 = maritalb, Utdanning = eisced)

# Fjerne personer som ikke har svart på Tillit, Fødselsår, kjønn, Utdanning, husholdningsinntekt
data_cleaned <- subset(ESS_data, Tillit != 77 & Tillit != 88 & Tillit != 99)
data_cleaned <- subset(data_cleaned, yrbrn != 7777 & yrbrn != 8888 & yrbrn != 9999)


# Fjerne kategori 11 og 12 fra runde 1-3
data_cleaned <- data_cleaned %>%
  filter(!(inntektA %in% c(11, 12)))

# Sammenslå variabler
data_cleaned <- data_cleaned %>%
  mutate(sivil_status = coalesce(gift, gift1, gift2),
         Total_inntekt = coalesce(inntektA, inntektB)) %>%
  select(-c(gift, gift1, gift2, inntektA, inntektB, proddate))


# Mappe landkoder
cntry <- unique(data_cleaned$cntry)
cntry2 <- unique(Gini_data$...1)

gini_data <- Gini_data %>%
  rename(cntry = ...1) %>%
  select(-c(...12))

land_kode_mapping <- c("Belgium" = "BE", "Norway" = "NO", "Finland" = "FI", "France" = "FR",        
                       "Germany" = "DE", "Hungary" = "HU", "Ireland" = "IE", "Netherlands" = "NL",
                       "Poland" = "PL", "Portugal" = "PT", "Slovenia" = "SI", "Spain" = "ES",
                       "Sweden" = "SE", "Switzerland" = "CH", "United Kingdom" = "GB")

# Erstatte landkoder i gini_data
gini_data$cntry <- ifelse(gini_data$cntry %in% names(land_kode_mapping), land_kode_mapping[gini_data$cntry], gini_data$cntry)

# Pivotere Gini data
gini_data <- pivot_longer(gini_data, cols = -cntry, names_to = "year", values_to = "Gini_index")

# Fikse data_cleaned
data_cleaned$year <- 2002 + 2 * (data_cleaned$essround - 1)

# Merge data
data_combined <- merge(data_cleaned, gini_data, by = c("cntry", "year"))

# Rydde og endre til norsk
data_combined <- data_combined %>%
  rename(År = year, land = cntry, id = name, kjønn = gndr, Fødselsår = yrbrn) %>%
  select(-c(essround))

# Konvertere 'Gini_index' fra character til numeric
data_combined$Gini_index <- as.numeric(data_combined$Gini_index)

# Sjekk for NAs etter konverteringen
sum(is.na(data_combined$Gini_index))


#Starter på 2008 på grunn av innhentingsvalg
data_combined<-data_combined %>%
  filter(År >= 2008)

# Utforskende dataanalyse
summary(data_combined$Gini_index)
summary(data_combined$Tillit)


#Fjerner de som ikke har svart
data_combined <- subset(data_combined, Total_inntekt != 77 & Total_inntekt != 88 & Total_inntekt != 99 & 
                        kjønn != 9 &
                        Utdanning != 77 & Utdanning != 88 & Utdanning != 99 & Utdanning !=55 & Utdanning !=0 &
                        sivil_status != 77 & sivil_status != 88 & sivil_status != 99 )
data_combined<-data_combined%>%select(-sivil_status)



sum_table <- sumtable(data_combined, labels = TRUE)

#Kjønnsdummy, fra 1 for mann og 2 for kvinner til 1 mann og 0 kvinner
# Kjønnsdummy (1 for mann, 0 for kvinne)
data_combined$Mann <- ifelse(data_combined$kjønn == 1, 1, 0)

#Utdanning til kategorier-> 6&7=høyere Utdanning, 4&5=Middels Utdanning, 1-3=lav Utdanning
data_combined <- data_combined %>%
  mutate(Utdanning = case_when(
    Utdanning %in% c(6, 7) ~ "Høyere Utdanning",
    Utdanning %in% c(4, 5) ~ "Middels Utdanning",
    Utdanning %in% c(1, 2, 3) ~ "Lav Utdanning"
  )) %>%
  mutate(Utdanning = fct_relevel(Utdanning, "Høyere Utdanning"))

# siste linje sikrer at 'Høyere Utdanning' er referansenivå


#År til tidsdummy 0,2,4 i stede for 2008, 2010, 2012

# Beregne tid basert på startåret 2008, 2008 blir 0 og dermed referanseår
data_combined <- data_combined %>%
  mutate(Tid = År - 2008)

#Ser at det er 2579 NAs i Gini_index
data_combined%>%count(Gini_index)

#Fjerner NA
data_combined<- data_combined%>%
  na.omit(data_combined)


# Lage dummyvariabler for land, med Norge som referanseland
#data_combined <- data_combined %>%
#  mutate(land = relevel(factor(land), ref = "NO"))


# Kjør regresjonsmodellen

mod1 <- lm(Tillit ~ Gini_index+Total_inntekt +Utdanning+ Mann+Tid, data = data_combined) #Fødselsår fucker oss over, intercept blir over 10 som ikke gÅr ettersom Tillit måles i 1-10


# Sammendrag av modellen
summary(mod1)

tab_model(mod1)


# Fjern landvariabelen fra data_combined for sumtable
data_combined_no_land <- data_combined %>% 
  select(-land, -kjønn, -Fødselsår, -År)
data_combined_no_land<- data_combined_no_land%>%
  rename(Kjønn=Mann)



# Lag en oversiktlig tabell med alle variablene som er tatt med i analysen, uten land
sum_table <- sumtable(data_combined_no_land, labels = TRUE) 



#FJERN PCTL 
st(data_combined_no_land, col.breaks = 5,
   summ = list(
     c('notNA(x)','mean(x)','sd(x^2)','min(x)','max(x)'),
     c('i')
   ),
   summ.names = list(
     c('N','Gj.snitt','Std.avvik','Min','Max'),
     c('i')
   ))






# Beregn antall unike land
num_countries <- length(unique(data_combined$land))

# Beregn antall observasjoner
num_observations <- nrow(data_combined_no_land)

# Bruke stargazer for å lage en tabell
stargazer_output <- capture.output(stargazer(data_combined_no_land, type = "text", 
                                             title = "Descriptive Statistics", digits = 3, 
                                             summary.stat = c("min", "max")))

# Legg til antall land og antall observasjoner manuelt til stargazer-output
stargazer_output <- append(stargazer_output, paste("Antall land:", num_countries), after = length(stargazer_output)-1)
stargazer_output <- append(stargazer_output, paste("Antall observasjoner:", num_observations), after = length(stargazer_output)-2)

# Print stargazer-output med antall land og antall observasjoner inkludert
cat(paste(stargazer_output, collapse = "\n"))




#Fordeling av Tillit
Tillit_bar<-data_combined%>% count(Tillit)


ggplot(Tillit_bar, aes(x = Tillit, y = n)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Set3") +
    scale_x_continuous(breaks = seq(0, 10, 1)) +
    scale_y_continuous(breaks = seq(0, 27500, 5000))+
    labs(title = "Fordeling av tillit",
         x = "Tillit",
         y = "Antall") +
    theme_minimal()




# Beregn gjennomsnittlig Gini-indeks per År for alle landene samlet
avg_gini <- data_combined %>%
  group_by(År) %>%
  summarise(gjennomsnittlig_gini = mean(Gini_index, na.rm = TRUE))

# Beregn gjennomsnittlig Tillit per År for alle landene samlet
avg_Tillit <- data_combined %>%
  group_by(År) %>%
  summarise(gjennomsnittlig_Tillit = mean(Tillit, na.rm = TRUE))

# Plot gjennomsnittlig Gini-indeks over tid
gini_plot <- ggplot(avg_gini, aes(x = År, y = gjennomsnittlig_gini)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(breaks = seq(28.6, 31, 0.2))+
  labs(title = "Gjennomsnittlig gini-indeks over tid",
       x = "År",
       y = "Gjennomsnittlig gini-indeks") +
  theme_minimal()

# Plot gjennomsnittlig Tillit over tid
Tillit_plot <- ggplot(avg_Tillit, aes(x = År, y = gjennomsnittlig_Tillit)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(breaks = seq(4.8, 5.6, 0.1))+
  labs(title = "Gjennomsnittlig tillit over tid",
       x = "År",
       y = "Gjennomsnittlig tillit") +
  theme_minimal()

# Vis plottene
print(gini_plot)
print(Tillit_plot)

library(gridExtra)
# Plasser plottene ved siden av hverandre
grid.arrange(gini_plot, Tillit_plot, ncol = 2)


data_2014_avg <- data_combined %>%
  filter(År == 2014) %>%
  group_by(land) %>%
  summarise(gjennomsnittlig_Tillit = mean(Tillit, na.rm = TRUE),
            Gini_index = mean(Gini_index, na.rm = TRUE))

correlation <- cor(data_2014_avg$Gini_index, data_2014_avg$gjennomsnittlig_Tillit, use = "complete.obs")
correlation

Tillit2014gini<-ggplot(data_2014_avg, aes(x = Gini_index, y = gjennomsnittlig_Tillit, label = land)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dotted") +
  geom_text(vjust = -.2, hjust = 0.5, show.legend = FALSE) +
  labs(title = paste("Gjennomsnittlig sosial tillit og Gini-indeks for alle land i 2014"),
       x = "Gini-indeks",
       y = "Sosial tillit") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Korrelasjon: ", round(correlation, 2)), 
           hjust = 4, vjust = 30, size = 4, color = "red", parse = FALSE)
Tillit2014gini


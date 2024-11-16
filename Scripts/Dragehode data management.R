# Script dataanalyses Effektoverv. Dragehode #
  ## Ruben Erik Roos, Marianne Evju ##
      ### NINA ###

#NB R may not recognize Norwegian letters in newer versions, so before import, the column names were changed in the excel file
##C& -> ae
##C8 -> oe
##C% -> aa
Sys.setlocale(locale='no_NB.utf8')

#### Install packages ####
library(tidyverse)
library(readxl)
library(vegan)
library(car)
library(goeveg)
library(ggrepel)

#### Load data ####
getwd()
env <- read_excel("Data/S123_Dragehode_2023_copy.xlsx", sheet='surveyPoint_0')#loads general data
species <- read_excel("Data/S123_Dragehode_2023_copy.xlsx", sheet='art_1')#loads species registration data

#### Data cleaning ####

#What we basically want to do is join the species df to the env file, joining them by "ParentGlobalID", we then want to clean this datafile so that it can be used for analyses

env <- env %>% 
  rename(ParentGlobalID = GlobalID)

df <- env %>% left_join(., species, by = "ParentGlobalID", keep = FALSE)

#filter out some of the data we do not need
df <- df %>% 
  select(-c('ObjectID.x', 
         'Klokkeslett start:', 
         'Analysert av:', 
         'Vaerforhold', 
         'Kartleggingsenhet 1:5000 i ruten', 
         'hovedtype_1m2', 
         'ke_beskrivelse_1m2', 
         'Hvilken type GPS brukes?', 
         'Noeyaktighet', 
         'Kommentarer:...15', 
         'Kommentarer:...16', 
         'veg_html_rows', 
         'Arter registrert:', 
         'Kommentarer:...24', 
         'Er det satt ned fastmerker?', 
         'Klokkeslett slutt:', 
         'Kommentarer:...38',
         'CreationDate.x', 
         'Creator.x', 
         'EditDate.x', 
         'Editor.x', 
         'ObjectID.y', 
         'GlobalID', 
         'Smaarutene nr.', 
         'Dekning %',
         'sp_species', 
         'ruternr_1':'ruternr_16', 
         'veg_html_row',
         'Creator.y', 
         'EditDate.y',
         'Editor.y', 
         'CreationDate.y'))

str(df) #check

#Now we can rename columns so they make sense, remove spaces, etc.
df <- df %>% 
  rename(Date = `Registreringsdato:`, 
         Polygon_ID = `Polygon ID`, 
         Plot_nr = `Rute-ID`,
         Treatment_2023 = `Skjoetsel - rute`, 
         Cover_plants = `Dekning % av karplanter i feltsjikt`, 
         Cover_plants_summed = `Total dekning % av arter registrert`, 
         Dragehode_seedlings = `Antall dragehoder: smaaplanter`,
         Dragehode_vegetative = `Antall dragehoder: vegetative planter`, 
         Dragehode_fertile = `Antall dragehoder: fertile`, 
         Cover_bryophytes = `Dekning % av bunnsjikt`,
         Cover_litter = `Dekning % av stroe`,
         Cover_rock = `Dekning % av grus/stein/berg/fjell`, 
         Cover_bare_soil = `Dekning % av bar jord`, 
         Cover_woody_plants = `Dekning % av vedplanter i feltsjikt`, 
         Cover_shrubs = `Dekning % av vedplanter i busksjikt`, 
         Cover_trees = `Dekning % tresjikt`, 
         Veg_height_1 = `Vegetasjonshoeyde maaling 1`, 
         Veg_height_2 = `Vegetasjonshoeyde maaling 2`, 
         Veg_height_3 = `Vegetasjonshoeyde maaling 3`, 
         Veg_height_4 = `Vegetasjonshoeyde maaling 4`, 
         Species = `Navn`, 
         Coor_x = `x`, 
         Coor_y = `y`,
         Cover_species = `sp_dekning`) %>% 
  unite("Plot_ID", Polygon_ID:Plot_nr, remove = FALSE, sep = "-") %>% #creates a unique identifier for each plot
  rowwise() %>% 
  mutate(Veg_height_mean = mean(c(Veg_height_1:Veg_height_4))) %>% 
  group_by(Plot_ID) %>% 
  mutate(Species_richness = n_distinct(Species)) %>% #adds species count as this didn't follow from the S125 file
  separate(Polygon_ID, c('Locality', 'Zone', 'Treatment', 'Treatment_dragehode'), sep = "-")

#In 2023, all of the plots were analysed before any mowing was performed, so Treament_2023 should be "none" for all plots here
unique(df$Treatment_2023)#This does not make sense, no plot was mown before analyses in 2023
df <- df %>% 
  mutate(Year = format(Date, format="%Y"),  #creates a column with year
          Treatment_2023 = if_else(Year == 2023, 'none', NA))  #creates column with treatment for 2023

#Rename some factor levels
df$Treatment <- recode_factor(df$Treatment, IS = "control", S = "mowing")
df$Treatment_dragehode <- recode_factor(df$Treatment_dragehode, D = "present", ID ="absent")

#Export the dataset
getwd()
write.csv(df, "Data/2022_effekt_dragehode.csv")

#For further (ordination) analyses, we want a file that only contains plotID, species, and their cover - in wide format
df_species <- df %>% 
  select(Plot_ID, Species, Cover_species) %>% 
  spread(Species, Cover_species) %>% 
  ungroup() %>% 
  select(-Plot_ID) %>% 
  mutate_if(is.character, as.numeric)
df_species <- df_species %>% replace(is.na(.), 0)
str(df_species)  

df_env <- df %>% 
  select(-c(Species, Cover_species)) %>% 
  group_by(Plot_ID) %>% 
  unique() %>%  #creates a datafile that contains all info except the species
  ungroup() %>% 
  select(c(Plot_ID, Treatment, Treatment_dragehode))

df_env$Plot_ID <- as.factor(df_env$Plot_ID)

#### A priori differences in species richness by Treatment and presence of dragehode ####

#between treatments
df %>% 
  group_by(Treatment) %>% 
  summarise(mean = mean(Species_richness))

#test difference with anova
aov.treatment <- aov(Species_richness ~ Treatment, data = df)
summary(aov.treatment)
plot(aov.treatment, 1)#some outliers present
leveneTest(Species_richness ~ Treatment, data = df)#variance across groups differs, do more robust test instead
kruskal.test(Species_richness ~ Treatment, data = df)#there actually is a difference 

df %>% group_by(Treatment_dragehode) %>% 
  summarise(mean = mean(Species_richness))

aov.dragehode <- aov(Species_richness ~ Treatment_dragehode, data = df)
summary(aov.dragehode)
plot(aov.dragehode, 1)#some outliers present
leveneTest(Species_richness ~ Treatment_dragehode, data = df)#variance across groups differs, do more robust test instead
kruskal.test(Species_richness ~ Treatment_dragehode, data = df)#there actually is a difference 

#### Ordination analyses a priori ####
#Convert species matrix to relative abundances
species_rel <-         
  decostand(df_species, method = "total")
#Export this dataset
write.csv(species_rel, "Data/Dragehode_2023_species_rel.csv")

#Create bray-curtiss distance matrix
species_distmat <- 
  vegdist(species_rel, method = "bray")

#create easy to view matrix and save (!!)
species_distmat <- 
  as.matrix(species_distmat, labels = T)
write.csv(species_distmat, "Data/Dragehode_2023_species_distmat.csv")

#Run NMDS in Vegan
species_NMS <-
  metaMDS(species_distmat,
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)
#stress = 0.08, OK

#check goodness of fit and stress
goodness(species_NMS)
stressplot(species_NMS)
dimcheckMDS(species_distmat,
            distance = "bray",
            k =10,
            trymax = 500) #2 or 3 dimension is OK

#Visualisation
#Source: https://www.rpubs.com/RGrieger/545184

#create quick plot
plot(species_NMS, "sites") 
orditorp(species_NMS, "sites") 

attach(df_env)
ordiplot(species_NMS)

plot1 <- ordiplot(Ordination.model1)
sites.long1 <- sites.long(plot1, env.data=prikk_env)
head(sites.long1)

#Create a gg plot
#Fit the environmental variables to the NMDS
envfit <- envfit(species_NMS, df_env, permutations = 999) # this fits environmental vectors
spp.fit <- envfit(species_NMS, df_species, permutations = 999)

#create coordinates for sites
site.scrs <- as.data.frame(scores(species_NMS, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Dragehode = df_env$Treatment_dragehode, Treatment = df_env$Treatment) #add grouping variable "Larvespinn" to dataframe
#site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot

#create coordinates for species
spp.scrs <- as.data.frame(scores(spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05

#extract other environmental variables
env.scores <- as.data.frame(scores(envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores <- cbind(env.scores, env.variables = rownames(env.scores)) #and then gives them their names

env.scores <- cbind(env.scores, pval = envfit$vectors$pvals) # add pvalues to dataframe
#sig.env.scrs <- subset(env.scores.dune, pval<=0.05) #subset data to show variables significant at 0.05



#Create basic plot
nmds.plot.2023 <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(Dragehode), shape = factor(Treatment)), size = 2)+ #adds site points to plot, shape determined by presence or absence of Larvespinn
  coord_fixed()+
  theme_classic()+
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Dragehode", shape = "Behandling")+ # add legend labels for Larvespinn
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

#add important species
nmds.plot.2023 <- nmds.plot.2023 +
  geom_point(data = sig.spp.scrs, aes(x = NMDS1, y=NMDS2), colour = "grey50") +
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), , colour = "grey50",  cex = 3, direction = "both", segment.size = 0.25, max.overlaps = Inf)

#Change some labels to norwegian
nmds.plot.2023 <- nmds.plot.2023 +
  labs(colour = "Dragehode", shape = "Behandling") +
  scale_color_discrete(labels = c('Ja', 'Nei')) +
  scale_shape_discrete(labels = c('Kontroll', 'Skal slaas'))

#Export plot
ggsave("Output/Figures/NMDS_dragehode_2023.jpg",  plot = nmds.plot.2023,
       scale = 1,
       width = 200,
       height = 280,
       units = c("mm")) 


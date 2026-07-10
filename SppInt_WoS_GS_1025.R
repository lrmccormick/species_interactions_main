# Copy of SppInt_WoS_GS_0325.R file, but edited to accommodate Google scholar search as well
##### EXTRA CODE WAS DELETED HERE FOR SIMPLICITY. #####

# New 1025 file started 10/03/2025
# Added in new data- final papers analysis included


setwd("C:/Users/lmccormick/OneDrive - DOI/Documents/Research/R/species_interactions_main")

library("here")
library("ggplot2")
library("dplyr")
library("lubridate")
library("grid")
library("gridExtra")
library("viridis")
library("gtable")
library("ggmap")
library("maps")
library("mapdata")
library("ggnewscale")
library("tidyverse")
library("ggalluvial")
library("tidyr")
library("stringr")
#library("deltamapr")
library("sf")

# Choy fw figures:
library("RColorBrewer")
library("igraph")
library("R.utils")

library("rnaturalearth")
library("rnaturalearthdata")
# Example RData load
#load("~/Documents/SIO/SeapHOx DeLUX/RedTideData/LJ_20.05.12.RData")



#File load:


AP <- read.delim("Alluvial_WoS_GS_02.25.25.txt", header = TRUE, sep = "\t", quote = "\"", #file didn't change from Feb analysis
                 dec = ".", fill = TRUE, comment.char = "",
                 stringsAsFactors=FALSE)



gen <- read.delim("High_Level_info_032025.txt", header = TRUE, sep = "\t", quote = "\"", 
                  dec = ".", fill = TRUE, comment.char = "",
                  stringsAsFactors=FALSE)



FW <- read.delim("Spp_int_DF_Aug25.txt", header = TRUE, sep = "\t", quote = "\"", 
                 dec = ".", fill = TRUE, comment.char = "",
                 stringsAsFactors=FALSE)
# slightly older version: "Spp_int_DF_Mar25.txt"

# FW <- read.delim("Mini_SppInt_DF.txt", header = TRUE, sep = "\t", quote = "\"", 
#                  dec = ".", fill = TRUE, comment.char = "",
#                  stringsAsFactors=FALSE)


############# Alluvial Plot ---------------------------------------------------
AP$Location <- as.factor(AP$Location)
AP$Location <- ordered(AP$Location, levels= c("All + Pacific", "All", "Pacific + SFB", 
                                              "SFB + Rivers", "SFB", "SFB + Suisun", "Suisun", 
                                              "Suisun + Delta", "Delta",
                                              "Delta + Rivers", "Rivers"))

AP$Status <- as.factor(AP$Status)
AP$Status <- ordered(AP$Status, levels= c("Listed", "Native", "Non-native", "Multiple"))

AP$Trophic.level <- as.factor(AP$Trophic.level)
AP$Trophic.level <- ordered(AP$Trophic.level, levels= c("Apex", "Sec.consumer",
                                                        "Prim.consumer", "Producer", "Other", "Multiple"))


# Play around with using tidy to creater ap dataframe
# sum <- gen %>% 
#   group_by(GN_EFFORT_ID, SPECIES) %>% #Group by gillnet and species
#   count()  %>% 
#   filter(SPECIES==307) %>% 
#   arrange(desc(n))


# Alluvial Plot for 8.21.24 file
is_alluvia_form(as.data.frame(AP), axes = 1:3, silent = TRUE)

ggplot(as.data.frame(AP), aes(y= AP$Freq, axis1= AP$Location, axis2= AP$Status, 
                              axis3= AP$Trophic.level))+
  geom_alluvium(aes(fill= Location))+
  geom_stratum(width = 1/3, alpha = 0.25, fill= "white", color= "black")+
  geom_label(stat = "stratum", aes(label= after_stat(stratum)))+
  scale_x_discrete(limits= c("Location", "Status", "Trophic Level"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Number of Studies")


#Know what colors are: viridis(11)
#"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"
# SFB: "#2A788EFF"
#Suisun: "#22A884FF"
# Delta: "#7AD151FF"
# Rivers: "#FDE725FF"



########################## Species status figures (for presentations)----------
stat.all <- AP %>% 
  filter(Location == "All" | Location == "All + Pacific") %>% 
  group_by(Status) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
stat.all$Status <- ordered(stat.all$Status, levels = c("Listed", "Native", "Non-native", "Multiple"))

stat.sf <- AP %>% 
  filter(Location == "SFB" | Location == "Pacific + SFB" | Location == "SFB + Rivers"
         | Location == "SFB + Suisun") %>% 
  group_by(Status) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
stat.sf$Status <- ordered(stat.sf$Status, levels = c("Listed", "Native", "Non-native", "Multiple"))

stat.sui <- AP %>% 
  filter(Location == "Suisun" | Location == "SFB + Suisun"
         | Location == "Suisun + Delta") %>% 
  group_by(Status) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
stat.sui$Status <- ordered(stat.sui$Status, levels = c("Listed", "Native", "Non-native", "Multiple"))

stat.del <- AP %>% 
  filter(Location == "Delta" | Location == "Delta + Rivers"
         | Location == "Suisun + Delta") %>% 
  group_by(Status) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
stat.del$Status <- ordered(stat.del$Status, levels = c("Listed", "Native", "Non-native", "Multiple"))

stat.riv <- AP %>% 
  filter(Location == "Rivers" | Location == "Delta + Rivers"
         | Location == "SFB + Rivers") %>% 
  group_by(Status) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
stat.riv$Status <- ordered(stat.riv$Status, levels = c("Listed", "Native", "Non-native", "Multiple"))

ggplot(stat.sf, aes(x= Status, y= Perc))+
  geom_col(fill= "#440154FF")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Species Status")+
  ylab("Percent of Studies")+
  ylim(0,50) 

ggplot(stat.sui, aes(x= Status, y= Perc))+
  geom_col(fill= "#31688EFF")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Species Status")+
  ylab("Percent of Studies")+
  ylim(0,50)

ggplot(stat.del, aes(x= Status, y= Perc))+
  geom_col(fill= "#35B779FF")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Species Status")+
  ylab("Percent of Studies")+
  ylim(0,50)

ggplot(stat.riv, aes(x= Status, y= Perc))+
  geom_col(fill= "#FDE725FF")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Species Status")+
  ylab("Percent of Studies")+
  ylim(0,50)

# Colors [viridis(4)]
#SFB:"#440154FF" 
#Suisun: "#31688EFF" 
#Delta: "#35B779FF" 
#Rivers: "#FDE725FF"


################ Trophic level figures (for presentations) --------------------
troph.all <- AP %>% 
  filter(Location == "All" | Location == "All + Pacific") %>% 
  group_by(Trophic.level) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
troph.all$Trophic.level <- ordered(troph.all$Trophic.level, levels = c("Producer", "Prim.consumer", "Sec.consumer",  "Apex", "Multiple", "Other"))

troph.sf <- AP %>% 
  filter(Location == "SFB" | Location == "Pacific + SFB" | Location == "SFB + Rivers"
         | Location == "SFB + Suisun") %>% 
  group_by(Trophic.level) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
troph.sf$Trophic.level <- ordered(troph.sf$Trophic.level, levels = c("Producer", "Prim.consumer", "Sec.consumer",  "Apex", "Multiple", "Other"))

troph.sui <- AP %>% 
  filter(Location == "Suisun" | Location == "SFB + Suisun"
         | Location == "Suisun + Delta") %>% 
  group_by(Trophic.level) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
other <- c("Other", 0, 0)
troph.sui <- rbind(troph.sui, other)
troph.sui$frequency <-  as.numeric(troph.sui$frequency)
troph.sui$Perc <- as.numeric(troph.sui$Perc)
troph.sui$Trophic.level <- ordered(troph.sui$Trophic.level, levels = c("Producer", "Prim.consumer", "Sec.consumer",  "Apex", "Multiple", "Other"))

troph.del <- AP %>% 
  filter(Location == "Delta" | Location == "Delta + Rivers"
         | Location == "Suisun + Delta") %>% 
  group_by(Trophic.level) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
troph.del <- rbind(troph.del, other)
troph.del$frequency <-  as.numeric(troph.del$frequency)
troph.del$Perc <- as.numeric(troph.del$Perc)
troph.del$Trophic.level <- ordered(troph.del$Trophic.level, levels = c("Producer", "Prim.consumer", "Sec.consumer",  "Apex", "Multiple", "Other"))

troph.riv <- AP %>% 
  filter(Location == "Rivers" | Location == "Delta + Rivers"
         | Location == "SFB + Rivers") %>% 
  group_by(Trophic.level) %>% 
  summarise(frequency = sum(Freq), .groups = 'drop')%>%
  arrange(desc(frequency)) %>% 
  mutate(Perc = frequency / sum(frequency, na.rm=TRUE) * 100)
prim <- c("Prim.consumer", 0, 0)
prod <- c("Producer", 0, 0)
troph.riv <- rbind(troph.riv, prim, prod)
troph.riv$frequency <-  as.numeric(troph.riv$frequency)
troph.riv$Perc <- as.numeric(troph.riv$Perc)
troph.riv$Trophic.level <- ordered(troph.riv$Trophic.level, levels = c("Producer", "Prim.consumer", "Sec.consumer",  "Apex", "Multiple", "Other"))


ggplot(troph.sf, aes(x= Trophic.level, y= Perc))+
  geom_col(fill= "#440154FF")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Trophic Level")+
  ylab("Percent of Studies")+
  ylim(0,50)

ggplot(troph.sui, aes(x= Trophic.level, y= Perc))+
  geom_col(fill= "#31688EFF")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Trophic Level")+
  ylab("Percent of Studies")+
  ylim(0,50)

ggplot(troph.del, aes(x= Trophic.level, y= Perc))+
  geom_col(fill= "#35B779FF")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Trophic Level")+
  ylab("Percent of Studies")+
  ylim(0,50)

ggplot(troph.riv, aes(x= Trophic.level, y= Perc))+
  geom_col(fill= "#FDE725FF")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Trophic Level")+
  ylab("Percent of Studies")+
  ylim(0,50)









#Publication Year histogram

ggplot(gen, aes(x=gen$Publication.Year))+
  geom_histogram(binwidth = 1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Publication Year")+
  ylab("Number of Studies")



###########################   Matt Nobriga Fig - too much data for now ------------
library(ggplot2)
library(vistime)

# Timeline graphic for LTO - food web
# Example with multiple factors on the same line but with differing colors

timeline <- data.frame(Position = c("marsh reclamation for agriculture", "marsh restoration",
                                    "striped bass dominance", "clam",
                                    "SAV", "dams", "peak dam builds", "CVP exports",
                                    "combined exports", "Striped Bass focus",
                                    "Delta Smelt focus"),
                       Name = c("Landscape", "Landscape", "Introductions", "Introductions", "Introductions",
                                "Dams", "Dams", "Exports", "Exports",
                                "Monitoring", "Monitoring"),
                       start = c("1850-01-01", "1999-01-01", "1879-01-01", "1987-01-01", "1999-01-01", "1910-01-01",
                                 "1945-01-01", "1951-01-01", "1968-01-01",
                                 "1959-01-01", "1995-01-01"),
                       end = c("1920-01-01", "2025-01-01", "1986-12-31", "1998-12-31", "2025-01-01", "1944-12-31",
                               "1979-01-01", "1967-12-31", "2025-01-01",
                               "1994-12-01", "2025-01-01"))

gg_vistime(timeline, col.event = "Position", col.group = "Name", title = "Drivers of food web change in the Bay-Delta") +
  theme(text=element_text(size=15)) + labs(x="Year")

# Timeline graphic for LTO - critical habitat
# Example where each factor gets its own line but factors in the same group have the same color

crit.hab <- data.frame(Position = c("Sac River dredged to 7 ft", "Sac River dredged to 30 ft",
                                    "Deep Water Ship Channel",
                                    "Early SJR dredging", "SJ River dredged to 37 ft",
                                    "Marsh reclamation for agriculture", "Marsh restoration",
                                    "Suisun Marsh water infrastructure", "SAV increasing",
                                    "Gold Rush sediments", "Sediment from Bay Area development",
                                    "Era of peak dam construction", "Temperature increasing in the spring",
                                    "Declining turbidity", "CVP Friant Dam operations",
                                    "CVP Shasta Dam operations", "CVP Jones Pumping Plant operations",
                                    "CVP Folsom Dam operations", "SWP Oroville Dam operations",
                                    "SWP Banks Pumping Plant operations", "SWP SOD storage",
                                    "CVP New Melones Dam operations", "D-1641", "TUCOs"),
                       Name = c("PCE 1", "PCE 1", "PCE 1", "PCE 1", "PCE 1",
                                "PCE 1", "PCE 1", "PCE 1", "PCE 1",
                                "PCE 2", "PCE 2", "PCE 2", "PCE 2", "PCE 2",
                                "PCE 3", "PCE 3", "PCE 3", "PCE 3", "PCE 3",
                                "PCE 3", "PCE 3", "PCE 3", "PCE 3", "PCE 3"),
                       start = c("1899-01-01", "1928-01-01", "1949-01-01", "1913-01-01", "1928-01-01",
                                 "1850-01-01", "1999-01-01", "1979-01-01", "1980-01-01", "1850-01-01",
                                 "1900-01-01", "1910-01-01", "1970-01-01", "1995-01-01", "1944-01-01", "1945-01-01",
                                 "1951-01-01", "1956-01-01", "1968-01-01", "1968-01-01", "1965-01-01", "1979-01-01",
                                 "2000-01-01", "2014-06-01"),
                       end = c("1927-12-31", "1948-12-31", "1963-01-01", "1927-12-31", "1987-01-01",
                               "1920-01-01", "2025-01-01", "2025-01-01", "2025-01-01", "1910-01-01",
                               "1965-01-01", "1978-01-01", "2025-01-01","2025-01-01", "2025-01-01", "2025-01-01",
                               "2025-01-01", "2025-01-01", "2025-01-01","2025-01-01", "1999-01-01", "2025-01-01",
                               "2014-01-01", "2025-01-01"),
                       color = c("skyblue", "skyblue", "skyblue", "skyblue", "skyblue",
                                 "skyblue", "skyblue", "skyblue", "skyblue",
                                 "tan1", "tan1", "tan1", "tan1", "tan1",
                                 "lightgreen", "lightgreen", "lightgreen", "lightgreen", "lightgreen",
                                 "lightgreen", "lightgreen", "lightgreen", "lightgreen", "lightgreen"))#; crit.hab

gg_vistime(crit.hab, optimize_y = FALSE, linewidth = 5, col.event = "Position", col.group = "Name",
           title = "Changes to delta smelt critical habitat PCEs")
theme(text=element_text(size=15)) + labs(x="Year")


# So to do this- take show year on x axis, y axis is species type? Or color? Want to also display location
# have species category be y axis, color be location

# Separate rows based on semicolons, make new df
time <- gen %>%
  separate_rows(Yr.Range, sep = ";|,") %>% #separators are comma and semi colon
  mutate(Yr.Range = str_trim(Yr.Range)) # removes space before term and at end of term

#unique(gen$)
unique(time$Yr.Range)

# add in a column for start date and end date
# Splitting the column into start and end dates
time <- time %>%
  mutate(
    Yr.Range = ifelse(Yr.Range == "", NA, Yr.Range),  # Convert blanks to NA
    start_year = ifelse(is.na(Yr.Range), NA, sub("^(\\d{4}).*", "\\1", Yr.Range)),
    end_year = ifelse(is.na(Yr.Range), NA, ifelse(grepl("-", Yr.Range), sub(".*-(\\d{4})", "\\1", Yr.Range), start_year)),
    start = ifelse(is.na(start_year), NA, as.POSIXct(paste0(start_year, "-01-01"), format = "%Y-%m-%d")),
    end = ifelse(is.na(end_year), NA, as.POSIXct(paste0(end_year, "-12-30"), format = "%Y-%m-%d"))
  )

# Makes a hard-to see plot that's not very interesting
gg_vistime(time,col.event = "Status", col.group = "Sp.Cat") + #title = "Drivers of food web change in the Bay-Delta"
  theme(text=element_text(size=15)) + labs(x="Year")








#################### Int. type histogram--------------------------------

int <- FW %>%
  separate_rows(Int.type, sep = ";|,") %>% #separators are comma and semi colon
  mutate(Methods = str_trim(Int.type))

ggplot(int, aes(x=int$Int.type))+
  geom_bar(stat= "count")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Interaction Type")+
  ylab("Number of Studies") #Not interesting

int <- FW %>%
  separate_rows(Int, sep = ";|,") %>% #separators are comma and semi colon
  mutate(Int = str_trim(Int))

int2 <-int %>%
  separate_rows(Int.cat, sep = ";|,") %>% #separators are comma and semi colon
  mutate(Int.cat = str_trim(Int.cat))

# order interactions pos-neg


ggplot(int2, aes(x=int2$Int))+
  geom_bar(stat= "count")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Interaction Type")+
  ylab("Number of Interactions")


#Interaction Types figure

# Just check
i <- int %>%
  count(Int, sort = TRUE)

# Clean up
int2$Int[which(int2$Int == "predation")] <- "exploitation"
int2$Int.cat[which(int2$Int.cat == "grazing")] <- "consumptive"
int2$Int.cat[which(int2$Int.cat == "consumtive")] <- "consumptive"

int2$Int <- as.factor(int2$Int)
int2$Int <- ordered(int2$Int, levels = c("mutualism", "commensalism", "neutral", "exploitation",
                                         "amensalism", "competition"))


ggplot(int2, aes(x=int2$Int, fill= int2$Int.cat))+
  geom_bar(stat= "count")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_viridis(discrete = TRUE, name= "Details")+
  xlab("Interaction Type")+
  ylab("Number of Interactions")





################# Methods data clean and plot -------------------------------

# Separate rows based on semicolons
methods <- gen %>%
  separate_rows(Methods, sep = ";|,") %>% #separators are comma and semi colon
  mutate(Methods = str_trim(Methods)) # removes space before term and at end of term


#Clean up: Removing duplicates and binning into bigger categories

methods$Methods[which(methods$Methods == "net collection" |
                        methods$Methods == "Seines" |
                        methods$Methods == "seining" |
                        methods$Methods == "trawls" |
                        methods$Methods == "trawling"|
                        methods$Methods == "plankton tows" |
                        methods$Methods == "net sampling"|
                        methods$Methods == "Beach seining" |
                        methods$Methods == "Beach seines" |
                        methods$Methods == "gill nets" |
                        methods$Methods == "net tows"|
                        methods$Methods == "CDFW Bay Study"|
                        methods$Methods == "20-mm survey"|
                        methods$Methods == "CDFW townet survey"|
                        methods$Methods == "Fall midwater trawl"|
                        methods$Methods == "USFWS Beach seine study"|
                        methods$Methods == "beach seine"|
                        methods$Methods == "beach seines"|
                        methods$Methods == "plankton net collections"|
                        methods$Methods == "light traps" |
                        methods$Methods == "zooplankton collections" |
                        methods$Methods == "Net tows"
)] <- "net collections"

methods$Methods[which(methods$Methods == "stomach contents" |
                        methods$Methods == "stomach contents analysis" |
                        methods$Methods == "Stomach contents analysis" |
                        methods$Methods == "Stomach content analysis"
)] <- "stomach content analysis"

methods$Methods[which(methods$Methods == "fecal analysis" |
                        methods$Methods == "fecal collection" |
                        methods$Methods == "pellet collection"
)] <- "fecal analysis"

methods$Methods[which(methods$Methods == "water flow data" |
                        methods$Methods == "water quality samples" |
                        methods$Methods == "water quality" |
                        methods$Methods == "water quality measurements"|
                        methods$Methods == "water quality measurements data"|
                        methods$Methods == "flow measurements"|
                        methods$Methods == "chlorophyll concentration"|
                        methods$Methods == "nutrient samples"|
                        methods$Methods == "nitrogen analysis"|
                        methods$Methods == "Water samples"|
                        methods$Methods == "Freshwater flow data"
)] <- "water quality data"

methods$Methods[which(methods$Methods == "abundance counts" |
                        methods$Methods == "abundance transects" |
                        methods$Methods == "catch data"|
                        methods$Methods == "population surveys"|
                        methods$Methods == "rodent abundance surveys"
)] <- "abundance"

methods$Methods[which(methods$Methods == "predation experiments" |
                        methods$Methods == "grazing experiments" |
                        methods$Methods == "Mesocosm herbivory experiments"
)] <- "feeding experiments"

methods$Methods[which(methods$Methods == "predator removal experiments" |
                        methods$Methods == "predator removal actions"
)] <- "predator removal"

methods$Methods[which(methods$Methods == "caging experiment"|
                        methods$Methods == "predator exclusion"
)] <- "predator exclusion"


methods$Methods[which(methods$Methods == "Animal collections" |
                        methods$Methods == "animal collection" |
                        methods$Methods == "Electrofishing" |
                        methods$Methods == "Collections"|
                        methods$Methods == "Hook and line"|
                        methods$Methods == "hook and line"|
                        methods$Methods == "benthic invertebrate sampling"|
                        methods$Methods == "oyster dredge"|
                        methods$Methods == "oyster surveys"|
                        methods$Methods == "electrofishing"
)] <- "animal collections"

methods$Methods[which(methods$Methods == "visual identification" |
                        methods$Methods == "visual encounter surveys" |
                        methods$Methods == "Visual encounter surveys" |
                        methods$Methods == "trail use monitoring" |
                        methods$Methods == "predation observation" |
                        methods$Methods == "listening stations"|
                        methods$Methods == "Nest observation" |
                        methods$Methods == "Nest monitoring" |
                        methods$Methods == "Cameras" |
                        methods$Methods == "cameras" |
                        methods$Methods == "observations" |
                        methods$Methods == "photography"|
                        methods$Methods == "Observation"|
                        methods$Methods == "Nest surveys"|
                        methods$Methods == "nest monitoring"|
                        methods$Methods == "observing"
)] <- "observation"

methods$Methods[which(methods$Methods == "selenium concentrations" |
                        methods$Methods == "selenium concentratoins" |
                        methods$Methods == "total PCB concentrations" |
                        methods$Methods == "selenium bioaccumulation" |
                        methods$Methods == "toxin measurements"|
                        methods$Methods == "metal concentrations"|
                        methods$Methods == "mercury analysis"|
                        methods$Methods == "PFAS concentrations"|
                        methods$Methods == "mercury concentrations"|
                        methods$Methods == "contaminants analysis"
)] <- "bioaccumulation"

methods$Methods[which(methods$Methods == "sonar" |
                        methods$Methods == "raio-telemetry" |
                        methods$Methods == "radio telemetry"|
                        methods$Methods == "radio-telemetry"|
                        methods$Methods == "radio tracking"|
                        methods$Methods == "acoustic tags"|
                        methods$Methods == "Radio marking (tracking)"|
                        methods$Methods == "acoustic telemetry"|
                        methods$Methods == "GPS tracking"
)] <- "telemetry"

methods$Methods[which(methods$Methods == "stable isotopes" |
                        methods$Methods == "stable isotpoes" |
                        methods$Methods == "isotope enrichment experiments" |
                        methods$Methods == "C:N ratios"
)] <- "stable isotope analysis"

methods$Methods[which(methods$Methods == "salvage data" |
                        methods$Methods == "salvage collection"
)] <- "salvage"

methods$Methods[which(methods$Methods == "otolith aging" |
                        methods$Methods == "otolith identification"
)] <- "otolith geochemistry"

methods$Methods[which(methods$Methods == "published data" |
                        methods$Methods == "published studies" |
                        methods$Methods == "previously published data" |
                        methods$Methods == "literature"|
                        methods$Methods == "Review"|
                        methods$Methods == "long-term datasets"
)] <- "review"

methods$Methods[which(methods$Methods == "plant measurements" |
                        methods$Methods == "plant measures" |
                        methods$Methods == "plant samples"|
                        methods$Methods == "SAV biomass surveys" |
                        methods$Methods == "SAV sampling"|
                        methods$Methods == "vegetation survey"
)] <- "vegetation surveys"

methods$Methods[which(methods$Methods == "modeling" |
                        methods$Methods == "Modeling"|
                        methods$Methods == "multi-variate autoregressive modeling" |
                        methods$Methods == "patch occupancy models" |
                        methods$Methods == "primary production models" |
                        methods$Methods == "primary production model" |
                        methods$Methods == "mark-recapture models" |
                        methods$Methods == "ecosystem metabolism model"|
                        methods$Methods == "correlations"|
                        methods$Methods == "bioenergetics models"|
                        methods$Methods == "bioenergetic models"
)] <- "models"

methods$Methods[which(methods$Methods == "eDNA" |
                        methods$Methods == "genetic analysis of stomach contents" |
                        methods$Methods == "PCR analysis"|
                        methods$Methods == "PCR"
)] <- "genetics"

methods$Methods[which(methods$Methods == "Behavioral experiments (predator cue avoidance)" |
                        methods$Methods == "CTmax experiments" |
                        methods$Methods == "behavioral experiments (predator avoidance)"|
                        methods$Methods == "colonization experiment" |
                        methods$Methods == "colonization experiments" |
                        methods$Methods == "cutting experiments" |
                        methods$Methods == "mesocosm experiments" |
                        methods$Methods == "enrichment experiments" |
                        methods$Methods == "greenhouse cross experiments" |
                        methods$Methods == "pathogen exposure experiments" |
                        methods$Methods == "field experiments"|
                        methods$Methods == "fertilization experiments"|
                        methods$Methods == "pollenation experiments"
)] <- "other experiments"

methods$Methods[which(methods$Methods == "sediment cores" |
                        methods$Methods == "soil samples" |
                        methods$Methods == "Ekman grab samples"|
                        methods$Methods == "Sediment cores" |
                        methods$Methods == "Sediment samples" |
                        methods$Methods == "benthic data"|
                        methods$Methods == "sediment samples" |
                        methods$Methods == "sediment characteristics" |
                        methods$Methods == "sediment salinity" |
                        methods$Methods == "sediment sampling" |
                        methods$Methods == "substrate analysis" |
                        methods$Methods == "box cores"
)] <- "benthic sampling"



m <- methods %>%
  count(Methods, sort = TRUE)

m.clean <- as.data.frame(rbind(m[1:16,], m[18:31,]))

m_sort <- m.clean %>%
  arrange(desc(n))

m_sort$Methods <- factor(m_sort$Methods, levels = m_sort$Methods)

#Methods histogram
ggplot(m_sort, aes(x=m_sort$Methods, y= m_sort$n))+
  geom_col()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Study Method")+
  ylab("Times Utilized")




################# Most common species --------------------------------------

s1 <- cbind.data.frame(FW$Title, FW$Authors, FW$Publication.Year, FW$Bay.Delta2, FW$Int.type, FW$Sp1.Phylum, FW$Sp1.Order, FW$Sp1.Cat, FW$Sp1.Common, FW$Sp1.Name)
s1$Sp <- 1
colnames(s1) <- c("Title", "Authors", "Year", "Loc", "Int.type", "Phylum", "Order", "Cat", "Common", "Name", "SpNo")

s2 <- cbind.data.frame(FW$Title, FW$Authors, FW$Publication.Year, FW$Bay.Delta2, FW$Int.type, FW$Sp2.Phylum, FW$Sp2.Order, FW$Sp2.Cat, FW$Sp2.Common, FW$Sp2.Name)
s2$Sp <- 2
colnames(s2) <- c("Title", "Authors", "Year", "Loc", "Int.type", "Phylum", "Order", "Cat", "Common", "Name", "SpNo")

spp <- rbind.data.frame(s1, s2)

spp_unique <- spp %>%
  distinct(Title, Loc, Common, SpNo, .keep_all = TRUE) # sort to list unique species per paper, keep all columns

# Only food web int

fw.spp <- as.data.frame(spp_unique[which(spp_unique$Int.type == "food web" |spp_unique$Int.type == "food web, contaminants"
                                         |spp_unique$Int.type == "food web, habitat"
                                         |spp_unique$Int.type == "food web; habitat"),])
# Now, split into regions

sp.all <- as.data.frame(fw.spp[which(fw.spp$Loc == "All" |fw.spp$Loc == "All + Pacific" ),])

sp.sfb <- as.data.frame(fw.spp[which(fw.spp$Loc == "SFB" |fw.spp$Loc == "SFB + Pacific"
                                     | fw.spp$Loc == "SFB + Suisun"
                                     | fw.spp$Loc == "SFB + Rivers"),])

sp.sui <- as.data.frame(fw.spp[which(fw.spp$Loc == "Suisun" |fw.spp$Loc == "Suisun + Delta"),])

sp.delta <- as.data.frame(fw.spp[which(fw.spp$Loc == "Delta"),])

sp.riv <- as.data.frame(fw.spp[which(fw.spp$Loc == "Delta + Rivers" | fw.spp$Loc == "Rivers" | fw.spp$Loc == "Rivers + SFB"),])


# f.sp.all <-sp.all %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))

f.sp.all <-sp.all %>% #see what the main species of study are
  group_by(Common, SpNo) %>%
  summarise(frequency = n(), .groups = 'drop')%>%
  arrange(desc(frequency))

f.sp.all <- factor(f.sp.all$Common, levels = f.sp.all$Common)

f.sp.sfb <-sp.sfb %>% #see what the main species of study are
  group_by(Common, SpNo) %>%
  summarise(frequency = n(), .groups = 'drop')%>%
  arrange(desc(frequency))

f.sp.sui <-sp.sui %>% #see what the main species of study are
  group_by(Common, SpNo) %>%
  summarise(frequency = n(), .groups = 'drop')%>%
  arrange(desc(frequency))

f.sp.delta <-sp.delta %>% #see what the main species of study are
  group_by(Common) %>%
  summarise(frequency = n(), .groups = 'drop')%>%
  arrange(desc(frequency))

f.sp.riv <-sp.riv %>% #see what the main species of study are
  group_by(Common) %>%
  summarise(frequency = n(), .groups = 'drop')%>%
  arrange(desc(frequency))



### dO THIS AGAIN BUT WITH SEPARATION OF sP 1 AND SP2
# Now, split into regions
# Use fw df
# s1 <- cbind.data.frame(fw$Title, fw$Authors, fw$Publication.Year, fw$Bay.Delta2, fw$Int.type, fw$Sp1.Phylum, fw$Sp1.Order, 
#                        fw$Sp1.Cat, fw$Sp1.Common, fw$Sp1.Name)
# s1$Sp <- 1
# colnames(s1) <- c("Title", "Authors", "Year", "Loc", "Int.type", "Phylum", "Order", "Cat", "Common", "Name", "SpNo")
# 
# 
# 
# f.sp.all <-sp.all %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.all <- factor(f.sp.all$Common, levels = f.sp.all$Common)
# 
# f.sp.sfb <-sp.sfb %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.sui <-sp.sui %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.delta <-sp.delta %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.riv <-sp.riv %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))



# 
# # Most common species
# 
# s1 <- cbind.data.frame(FW$Title, FW$Authors, FW$Publication.Year, FW$Bay.Delta2, FW$Int.type, FW$Sp1.Phylum, FW$Sp1.Order, FW$Sp1.Cat, FW$Sp1.Common, FW$Sp1.Name)
# s1$Sp <- 1
# colnames(s1) <- c("Title", "Authors", "Year", "Loc", "Int.type", "Phylum", "Order", "Cat", "Common", "Name", "SpNo")
# 
# s2 <- cbind.data.frame(FW$Title, FW$Authors, FW$Publication.Year, FW$Bay.Delta2, FW$Int.type, FW$Sp2.Phylum, FW$Sp2.Order, FW$Sp2.Cat, FW$Sp2.Common, FW$Sp2.Name)
# s2$Sp <- 2
# colnames(s2) <- c("Title", "Authors", "Year", "Loc", "Int.type", "Phylum", "Order", "Cat", "Common", "Name", "SpNo")
# 
# spp <- rbind.data.frame(s1, s2)
# 
# spp_unique <- spp %>%
#   distinct(Title, Loc, Common, SpNo, .keep_all = TRUE) # sort to list unique species per paper, keep all columns
# 
# # Only food web int
# 
# fw.spp <- as.data.frame(spp_unique[which(spp_unique$Int.type == "food web" |spp_unique$Int.type == "food web, contaminants"
#                                          |spp_unique$Int.type == "food web, habitat"
#                                          |spp_unique$Int.type == "food web; habitat"),])
# # Now, split into regions
# 
# sp.all <- as.data.frame(fw.spp[which(fw.spp$Loc == "All" |fw.spp$Loc == "All + Pacific" ),])
# 
# sp.sfb <- as.data.frame(fw.spp[which(fw.spp$Loc == "SFB" |fw.spp$Loc == "SFB + Pacific"
#                                      | fw.spp$Loc == "SFB + Suisun"
#                                      | fw.spp$Loc == "SFB + Rivers"),])
# 
# sp.sui <- as.data.frame(fw.spp[which(fw.spp$Loc == "Suisun" |fw.spp$Loc == "Suisun + Delta"),])
# 
# sp.delta <- as.data.frame(fw.spp[which(fw.spp$Loc == "Delta"),])
# 
# sp.riv <- as.data.frame(fw.spp[which(fw.spp$Loc == "Delta + Rivers" | fw.spp$Loc == "Rivers" | fw.spp$Loc == "Rivers + SFB"),])
# 
# 
# f.sp.all <-sp.all %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.all <-sp.all %>% #see what the main species of study are
#   group_by(Common, SpNo) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.all <- factor(f.sp.all$Common, levels = f.sp.all$Common)
# 
# f.sp.sfb <-sp.sfb %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.sui <-sp.sui %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.delta <-sp.delta %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.riv <-sp.riv %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))

# 
# 
# ### dO THIS AGAIN BUT WITH SEPARATION OF sP 1 AND SP2
# # Now, split into regions
# # Use fw df for Only food web interactions
# 
# fw <- as.data.frame(FW[which(FW$Int.type == "food web" |FW$Int.type == "food web, contaminants"
#                              |FW$Int.type == "food web, habitat"
#                              |FW$Int.type == "food web; habitat"),])
# 
# s1 <- cbind.data.frame(fw$Title, fw$Authors, fw$Publication.Year, fw$Bay.Delta2, fw$Sp1.Cat, fw$Sp1.Common, fw$Sp1.Name,
#                        fw$Category, fw$Int.type, fw$Int.cat, fw$Sp2.Cat, fw$Sp2.Common, fw$Sp2.Name)
# colnames(s1) <- c("Title", "Authors", "Year", "Loc", "Sp1.Cat", "Sp1.Common", "Sp1.Name", "Category", "Int.type", "Int.cat",
#                   "Sp2.Cat", "Sp2.Common", "Sp2.Name")
# 
# # Now, split into regions
# 
# sp.all <- as.data.frame(s1[which(s1$Loc == "All" |s1$Loc == "All + Pacific" ),])
# 
# sp.sfb <- as.data.frame(s1[which(s1$Loc == "SFB" |s1$Loc == "SFB + Pacific"
#                                  | s1$Loc == "SFB + Suisun"
#                                  | s1$Loc == "SFB + Rivers"),])
# 
# sp.sui <- as.data.frame(s1[which(s1$Loc == "Suisun" |s1$Loc == "Suisun + Delta"),])
# 
# sp.delta <- as.data.frame(s1[which(s1$Loc == "Delta"),])
# 
# sp.riv <- as.data.frame(s1[which(s1$Loc == "Delta + Rivers" | s1$Loc == "Rivers" | s1$Loc == "Rivers + SFB"),])
# 
# 
# 
# f.sp.all <-sp.all %>% #see what the main species of study are
#   group_by(Sp1.Common, Int.type, Int.cat, Sp2.Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.all <- factor(f.sp.all$Common, levels = f.sp.all$Common)
# 
# f.sp.sfb <-sp.sfb %>% #see what the main species of study are
#   group_by(Sp1.Common, Int.type, Int.cat, Sp2.Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.sui <-sp.sui %>% #see what the main species of study are
#   group_by(Sp1.Common, Int.type, Int.cat, Sp2.Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.delta <-sp.delta %>% #see what the main species of study are
#   group_by(Sp1.Common, Int.type, Int.cat, Sp2.Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.riv <-sp.riv %>% #see what the main species of study are
#   group_by(Sp1.Common, Int.type, Int.cat, Sp2.Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 




###### Go into depth on one species- which is most common overall?

# f.spp_unique <-spp_unique %>% #see what the main species of study are
#   group_by(Common) %>%
#   summarise(frequency = n(), .groups = 'drop')%>%
#   arrange(desc(frequency))
# 
# f.sp.all <- factor(f.sp.all$Common, levels = f.sp.all$Common)




### Detailed story about one species -initial try

#Striped bass:
strb <- FW %>% 
  filter(Sp1.Name == "Morone saxatilis" | Sp2.Name == "Morone saxatilis")

unique(strb$Sp1.Cat)

nanch <- FW %>% 
  filter(Sp1.Name == "Engraulis mordax" | Sp2.Name == "Engraulis mordax")

lgmb <- FW %>% 
  filter(Sp1.Name == "Micropterus salmoides" | Sp2.Name == "Micropterus salmoides")

# smaller dataframe with each each species combinations weighted to how often they occur
t1 <- strb %>%
  #distinct(Sp1.Common, Sp2.Common, Category, Int, .keep_all = TRUE) %>% 
  group_by(Sp1.Common, Sp1.Cat, Sp2.Common, Sp2.Cat, Category, Int) %>%
  summarise(frequency = n())%>%
  arrange(desc(frequency))

write.csv(t1, "./t1_data.csv", row.names = FALSE)

#t2 <- select(t1, Sp1.Common, Sp2.Common, frequency)

#Create a weighted graph:
g <- graph_from_data_frame(t1[1:28,], directed = TRUE)

plot(g,
     edge.width = E(g)$frequency * 1, # Edge width based on weight
     #edge.lty = E(g)$Category,
     vertex.size = 28,
     vertex.color = "lightblue",
     vertex.label.color = "black",
     #edge.color = E(g)$Int, 
     edge.color = "gray50"#,
     #layout = layout_in_circle(g)
     #layout = layout_with_drl(g)
)

################# Food web diagrams ------------------------------------------
# Following Choy et al. 2017 data...

# Make nodes and links (edges) files
# for nodes, need a file that has "from" and "to" groups (e.g. FISH, amphipods, etc); weight (no. int?); "from group" and "to group" label
# A	B	C	D	E
# 1	from	to	weight	from_group	to_group
# 2	fwg5	fwg1	1	ctenophore	calycophoran siphonophore
# 3	fwg9	fwg1	2	medusa general	calycophoran siphonophore

# Make dataset specific for use
# I need a data from that separates unique combos of spp 1 and species 2. With a count of the times it was documented. Separate into just fw effects.
fw.sp <- cbind.data.frame(FW$Title, FW$Authors, FW$Publication.Year, FW$Bay.Delta2, FW$Int.type, FW$Int, FW$Int.cat, FW$Int.info, FW$Sp1.Phylum, FW$Sp1.Order, FW$Sp1.Cat, FW$Sp1.Common, FW$Sp1.Name,
                          FW$Sp2.Phylum, FW$Sp2.Order, FW$Sp2.Cat, FW$Sp2.Common, FW$Sp2.Name)

colnames(fw.sp) <- c("Title", "Authors", "Year", "Loc", "Int.type","Int.def", "Int.cat", "Int.info", "Sp1.Phylum", "Sp1.Order", "Sp1.Cat", "Sp1.Common", "Sp1.Name",
                     "Sp2.Phylum", "Sp2.Order", "Sp2.Cat", "Sp2.Common", "Sp2.Name")

# To create the main species list for the dropdown menu
sp.list1 <-cbind.data.frame(fw.sp$Sp1.Common, fw.sp$Sp1.Phylum, fw.sp$Sp1.Order, 
                            fw.sp$Sp1.Cat, fw.sp$Sp1.Name)
colnames(sp.list1) <- c("Common", "Phylum", "Order", "Cat", "Name")
sp.list2 <-cbind.data.frame(fw.sp$Sp2.Common, fw.sp$Sp2.Phylum, fw.sp$Sp2.Order, 
                            fw.sp$Sp2.Cat, fw.sp$Sp2.Name)
colnames(sp.list2) <- c("Common", "Phylum", "Order", "Cat", "Name")

sp.list<- rbind.data.frame(sp.list1, sp.list2)

sp.list <- sp.list %>% 
  arrange(Common) #arrange alphabetically 
sp.list <- unique(sp.list) # Remove duplicates
#sp.list <- sp.list[sp.list$Common != "" & !is.na(sp.list$Common), , drop=FALSE]
#save as csv
common.list <- as.data.frame(unique(sp.list$Common))
write_csv(sp.list, "sp.list.main.csv")
write_csv(common.list, "common.list.csv")


#This works to create a larger dataframe. May need to simplify (eg. only predation interactions, classify other types of interactions, etc.)
t1 <- fw.sp %>%
  #distinct(Sp1.Common, Sp2.Common, Category, Int, .keep_all = TRUE) %>% 
  group_by(Sp1.Common, Sp2.Common, Int.def) %>%
  summarise(frequency = n())%>%
  arrange(desc(frequency))




# Species profiles:

# narrow down to one species, e.g. Delta smelt
dsm <- fw.sp %>% 
  filter(Sp1.Name == "Hypomesus transpacificus" | Sp2.Name == "Hypomesus transpacificus")

d1 <- dsm %>% 
  group_by(Sp1.Common, Sp1.Cat, Sp2.Common, Sp2.Cat, Int.def) %>% 
  summarise(frequency= n()) %>% 
  arrange(desc(frequency))

# d2 <- dsm2 %>% 
#   group_by(Sp1.Common, Sp1.Cat, id.x, Sp2.Common, Sp2.Cat, id.y, Int.def) %>% 
#   summarise(frequency= n()) %>% 
#   arrange(desc(frequency))
# 
# #edit:
# d2[18,4] <- "Calanoid copepod"

# nodes <- cbind.data.frame(d2$id.x, d2$id.y, d2$frequency, d2$Sp1.Common, d2$Sp2.Common)
# colnames(nodes) <- c("from", "to", "weight", "from_group", "to_group")

# nodes2 <- cbind.data.frame(d1$Sp1.Cat, d1$Sp2.Cat, d1$frequency, d1$Sp1.Common, d1$Sp2.Common)
# colnames(nodes2) <- c("from", "to", "weight", "from_group", "to_group")


# #Edge data
# A	B	C	D
# 1	did	group	group_type	type_label
# 2	fwg1	calycophoran siphonophore	1	gelatinous animal
# 3	fwg10	narcomedusa	1	gelatinous animal
# 4	fwg11	mollusc other	2	mollusc
# 5	fwg12	physonect siphonophore	1	gelatinous animal

#unique(fw.sp$Sp1.Cat)

#dsm[38,17]<- "Calanoid copepod"


#Edit similar categories:
#For species 1
dsm$Sp1.Common2 <- dsm$Sp1.Common
dsm$Sp1.Common2[which(dsm$Sp1.Common2 == "Delta smelt larvae 12dph" |
                        dsm$Sp1.Common2 == "Delta smelt larvae 17dph" |
                        dsm$Sp1.Common2 == "Delta smelt larvae 31dph" |
                        dsm$Sp1.Common2 == "Delta smelt larvae 67dph" |
                        dsm$Sp1.Common2 == "Delta smelt larvae 145dph" |
                        dsm$Sp1.Common2 == "Delta smelt larvae" 
)] <- "Delta smelt ELS"

dsm$Sp1.Common2[which(dsm$Sp1.Common2 == "Largemouth bass <=175 mm" |
                        dsm$Sp1.Common2 == "Largemoth bass juveniles" |
                        dsm$Sp1.Common2 == "Largemouth bass juveniles" |
                        dsm$Sp1.Common2 == "Largemouth bass >175 mm"|
                        dsm$Sp1.Common2 == "Largemoth bass" |
                        dsm$Sp1.Common2 == "Largemouth bass"
)] <- "LMB"

dsm$Sp1.Common2[which(dsm$Sp1.Common2 == "Human"
)] <- "Humans"

dsm$Sp1.Common2[which(dsm$Sp1.Common2 == "Brazilian waterweed" |
                        dsm$Sp1.Common2 == "Submerged aquatic vegetation"
)] <- "SAV"

dsm$Sp1.Common2[which(dsm$Sp1.Common2 == "Water hyacinth"
)] <- "FAV"

dsm$Sp1.Common2[which(dsm$Sp1.Common2 == "Sacramento pikeminnow"
)] <- "Sac. pikeminnow"

dsm$Sp1.Common2[which(dsm$Sp1.Common2 == "Mississippi silverside"
)] <- "Miss. silverside"

dsm$Sp1.Common2[which(dsm$Sp1.Common2 == "Exopalaemon shrimp"
)] <- "Dec. shrimp"

# For spp 2
dsm$Sp2.Common2 <- dsm$Sp2.Common
dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Calanoid" |
                        dsm$Sp2.Common2 == "Calanoid copepodites" |
                        dsm$Sp2.Common2 == "Calanoid copepod (unid)" |
                        dsm$Sp2.Common2 == "Calanoid copepod" |
                        dsm$Sp2.Common2 == "Diaptomidae" 
)] <- "Calanoids"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Cyclopoid" |
                        dsm$Sp2.Common2 == "Cyclopoid copepod"|
                        dsm$Sp2.Common2 == "Acanthocyclops spp."
)] <- "Cyclopoids"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Delta smelt embryos and larvae"
)] <- "Delta smelt ELS"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Eurytemora copepodites" |
                        dsm$Sp2.Common2 == "Eurytemora " |
                        dsm$Sp2.Common2 == "Eurytemora nauplii" 
)] <- "Eurytemora"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Sinocalanus copepodites"
)] <- "Sinocalanus"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Limnoithona copepodites"
)] <- "Limnoithona"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Pseudodiaptomus copepodites" |
                        dsm$Sp2.Common2 == "Pseudodiaptomus nauplii" |
                        dsm$Sp2.Common2 == "Pseudodiaptomus copepodites" |
                        dsm$Sp2.Common2 == "Pseudodioptamus"
)] <- "Pseudodiaptomus"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Daphnia" |
                        dsm$Sp2.Common2 == "Cladoceran" | 
                        dsm$Sp2.Common2 == "Daphniidae" |
                        dsm$Sp2.Common2 == "Bosmina cladocerans" |
                        dsm$Sp2.Common2 == "Ceriodaphnia cladocerans" |
                        dsm$Sp2.Common2 == "Daphnia cladocerans" 
)] <- "Cladocerans"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Chironomid larvae" |
                        dsm$Sp2.Common2 == "Corixidae nymphs"
)] <- "Insect larvae"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Harpacticoid copepod"
)] <- "Harpacticoids"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Oithonid copepod"
)] <- "Oithonids"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "Corophiid amphipod"
)] <- "Corophiids"

dsm$Sp2.Common2[which(dsm$Sp2.Common2 == "NH4(+), DIN:DIP"
)] <- "Nutrients"


# Create a database for edge data
comb <- c(unique(dsm$Sp1.Common2), unique(dsm$Sp2.Common2))
list <- unique(comb)
cat <- data.frame(Cat= list)
cat <- cat[cat$Cat != "" & !is.na(cat$Cat), , drop=FALSE] # add in something that will remove rows with blanks or NA automatically

#cat <- cat[-c(30),, drop=FALSE] manually remove rows with blanks

#n <- cbind(c(seq(from = 1 , to= 16), 17, 17, 17, 17, 17, seq(from=18, to= 27), 17, seq(from= 28, to= 64)))
n <- seq(from = 1, to = nrow(cat))
id <- paste("fw", n, sep = "")
#id <- cbind(c("fw1", "fw2", "fw3", "fw4", "fw5", "fw6", "fw7", "fw8", "fw9", "fw10", "fw11"))
ID <- cbind(id, cat)

# New dataframe is dsm2
dsm2 <- dsm %>% 
  left_join(ID, by = c("Sp1.Common2" ="Cat"))

dsm2 <- dsm2 %>% 
  left_join(ID, by= c("Sp2.Common2" = "Cat"))
#colnames(dsm2[,19:20]) <- c("Sp1.Id", "Sp2.Id") # can't change this, so Sp1.id is id.x, sp2.id is id.y


# Important that you separate so it's not pulling a bunch of records from the same paper. This should give distinct spp in with # of studies showing that interaction
d2 <- dsm2 %>% 
  group_by(Title, Sp1.Common2, Sp1.Cat, id.x, Sp2.Common2, Sp2.Cat, id.y, Int.def) %>% 
  distinct(Title, id.x, id.y, Int.def, .keep_all = TRUE) 

#narrow down for plotting
d3 <- d2 %>% 
  group_by(Sp1.Common2, Sp1.Cat, id.x, Sp2.Common2, Sp2.Cat, id.y, Int.def) %>% 
  summarise(frequency= n()) %>% 
  arrange(desc(frequency))


#To find list of species prey items
dsm_prey <- d3 %>%
  filter(Sp1.Common2 == "Delta smelt" | Sp1.Common2 == "Delta smelt ELS" )

#To find list of species predators
dsm_pred <- d3 %>%
  filter(Sp2.Common2 == "Delta smelt" | Sp2.Common2 == "Delta smelt ELS" )

#To find list of species other interactions
dsm_int <- d3 %>%
  filter(Int.def != "exploitation" )

#edit:
#d2[18,4] <- "Calanoid copepod"
#ID <- cbind(id, cat)
vertices <- ID #vertices- contains unique spp
edges <- cbind.data.frame(d3$id.x, d3$id.y, d3$frequency, d3$Sp1.Common2, d3$Sp2.Common2, d3$Int.def) #edges- contains "to" and "from" data
colnames(edges) <- c("from", "to", "weight", "from_group", "to_group", "int")

#net <- graph_from_data_frame(d=nodes, directed= T, vertices = links)
net <- graph_from_data_frame(d = edges, directed = TRUE, vertices = vertices)
print(net, e= TRUE, v= TRUE)

######################
#Customize colors - for FULL color figure!
colorvalues <- c(
  "Delta smelt" = "turquoise4", "Delta smelt ELS" = "turquoise2",
  "LMB" = "dodgerblue4", "Striped bass" = "dodgerblue3", "Miss. silverside"= "dodgerblue2", "Wakasagi"= "dodgerblue1",
  "Channel catfish" = "deepskyblue2", "White catfish" = "deepskyblue2", "Bluegill sunfish"="deepskyblue2", "Sac. pikeminnow" ="deepskyblue2",
  "Chinook salmon" = "deepskyblue2", "Tule perch" ="deepskyblue2", "Threadfin shad" = "deepskyblue2", "Golden shiner"="deepskyblue2", "Shimofuri goby"="deepskyblue2",
  "Threespine stickleback"="deepskyblue2",
  "Pseudodiaptomus" = "darkorange", "Calanoids" = "darkorange", "Eurytemora" = "darkorange", "Sinocalanus"= "darkorange",
  "Humans" = "purple",
  "Cyclopoids" = "orange", "Limnoithona"= "orange", 
  "Harpacticoids"= "goldenrod2", "Oithonids" = "goldenrod2", "Copepods"= "goldenrod2",
  "Cladocerans" = "tomato", "Corophiids" = "tomato4", "Opossum shrimp"= "coral3", "Dec. shrimp"= "darksalmon", "Crustacean parts"= "darksalmon", 
  "Insect larvae"= "red",
  "SAV"= "green4", "FAV" = "green3",
  "Overbite clam"= "hotpink3",
  "Rotifer" = "slateblue3",
  "Detritus"="gray", "Nutrients" = "gray")


#Assign colors to cat type (if not defined, they are gray)
V(net)$color <- colorvalues[V(net)$Cat]
V(net)[is.na(V(net)$color)]$color <- adjustcolor("lightgray",.6)

#########################
# Assign edge colors based on predator or prey type
#E(net)$color <- adjustcolor(colorvalues[E(net)$from_group], 0.6)  # Color edges by predator
#E(net)[is.na(E(net)$color)]$color <- adjustcolor("lightgray",.6) # this doesn't work
# Set edges with missing colors to gray
#E(net)[is.na(E(net)$color)]$color <- "gray"


# Try to force all to be colored:
#E(net)$color <- "gray"

# Get edge colors based on predator
edge_colors <- colorvalues[E(net)$from_group] 

# Replace NAs with gray
edge_colors[is.na(edge_colors)] <- "gray"

# Assign to the edges
E(net)$color <- adjustcolor(edge_colors, 0.6) # this makes sure that lines are semi-transparent

# Alternative: Color edges by prey instead
# E(net)$color <- adjustcolor(colorvalues[E(net)$to_group], 0.6)


# I also want the interaction type to be added in:
# efine a mapping for linetypes based on interaction types
linetype_map <- c("exploitation" = 1,  # solid
                  "competition" = 2,  # dashed
                  "amensalism" = 4)  # dotted

# Assign linetypes to edges
E(net)$lty <- linetype_map[E(net)$int] 

# Replace NAs with solid lines (default)
#E(net)$lty[is.na(E(net)$lty)] <- 1 #don't need this now, but if you have NAs you could add


# Labels- instead of fw1... default
V(net)$label <- V(net)$Cat  # Use species type as the label
#V(net)$label <- substr(V(net)$cat, 1, 3)  # Shorten label to first 3 letters
V(net)$label.cex <- 0.9  # Reduce label size

# To use a species code label- could either incorporate it into the Sp1.Common2 (=Vertices$Cat) list:
#V(net)$label <- V(net)$code  # Use species code as label

#or could map them from a larger species code lookup file (likely easier in the larger scale things)
#species_codes <- c("Delta smelt" = "DS", "Delta smelt larvae" = "DSL", "Other species" = "OS")
#V(net)$label <- species_codes[V(net)$cat]  # Map species codes based on `cat`
#V(net)[is.na(V(net)$label)]$label <- "UNK"  # Set unknown codes to "UNK" (or leave blank "")

#fullnodes <-  V(net)$cat

#From Anela's paper- leave out for now
#V(net)$color <- adjustcolor("lightgray",.6)
#V(net)$color = colorvalues(fullnodes) #error- thinks colorvalues is a full function

# Need to adjust this so that the edges are more extreme
#From Anela's paper- original
#E(net)$width <- 2+E(net)$weight/4
E(net)$width <- 2 + (E(net)$weight * 2)  # Multiply weight by 2 (or another factor) to amplify the interactions in a proportional way

#V(net)[is.na(V(net)$color)]$color = adjustcolor("lightgray",.6)

#If you like the layout- comment these out. Otherwise this will randomly assign a layout
l <- layout_with_graphopt(net)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

#E(net)$edge.color =  adjustcolor(colorvalues[E(net)$from_group],.6)
par(mar = c(1, 0.5, 1, 1))  # Reduce margins (bottom, left, top, right)
plot.igraph(net,edge.arrow.size=0.03, edge.color=E(net)$color,
            edge.arrow.mode=2, rescale=TRUE, edge.curved=.4, layout=l, edge.lty = E(net)$lty,   # Apply linetypes
            vertex.label.color= "black",vertex.label.family="sans")  

# Basic legend
# legend("right", names(colorvalues), pch=21, col='#000000', lty = linetype_map, #this adds the linetypes into the legend
#        pt.bg=colorvalues, pt.cex=2, cex=0.7, bty='n', ncol=1) # could use coords for placement: x=0, y=-1.1

legend("right", 
       legend = c(names(colorvalues), names(linetype_map)),  # Combine colors & linetypes
       pch = c(rep(21, length(colorvalues)), rep(NA, length(linetype_map))),  # Circles for color, no symbol for lines
       pt.bg = c(colorvalues, rep(NA, length(linetype_map))),  # Background color for nodes
       col = c(rep("black", length(colorvalues)), rep("black", length(linetype_map))),  # Line color
       lty = c(rep(NA, length(colorvalues)), linetype_map),  # Linetype only for edges
       lwd = c(rep(NA, length(colorvalues)), rep(2, length(linetype_map))),  # Line width for linetypes
       pt.cex = 2,  # Make the color circles bigger
       cex = 0.7, 
       bty = "n", 
       ncol = 1)  # Set to 1 column for clarity


######## For PARTIAL COLOR FIG
####### PREY
colorvalues_prey <- c(
  "Delta smelt" = "turquoise4", "Delta smelt ELS" = "turquoise2",
  "Pseudodiaptomus" = "darkorange", "Calanoids" = "darkorange", "Eurytemora" = "darkorange", "Sinocalanus"= "darkorange",
  "Cyclopoids" = "orange", "Limnoithona"= "orange", 
  "Harpacticoids"= "goldenrod2", "Oithonids" = "goldenrod2", "Copepods"= "goldenrod2",
  "Cladocerans" = "tomato", "Corophiids" = "tomato4", "Opossum shrimp"= "coral3", "Crustacean parts"= "darksalmon", 
  "Insect larvae"= "red",
  "Rotifer" = "slateblue3",
  "Detritus"="peru")

#Assign colors to cat type (if not defined, they are gray)
V(net)$color <- colorvalues_prey[V(net)$Cat]
V(net)[is.na(V(net)$color)]$color <- adjustcolor("lightgray",.6)

#########################
# Assign edge colors based on predator or prey type

# Get edge colors based on predator
edge_colors <- colorvalues_prey[E(net)$from_group] 

# Replace NAs with gray
edge_colors[is.na(edge_colors)] <- "gray"

# Assign to the edges
E(net)$color <- adjustcolor(edge_colors, 0.6) # this makes sure that lines are semi-transparent


# I also want the interaction type to be added in:
# efine a mapping for linetypes based on interaction types
linetype_map <- c("exploitation" = 1,  # solid
                  "competition" = 2,  # dashed
                  "amensalism" = 4)  # dotted

# Assign linetypes to edges
E(net)$lty <- linetype_map[E(net)$int] 


# Labels- instead of fw1... default
V(net)$label <- V(net)$Cat  # Use species type as the label
#V(net)$label <- substr(V(net)$cat, 1, 3)  # Shorten label to first 3 letters
V(net)$label.cex <- 0.9  # Reduce label size


# Need to adjust this so that the edges are more extreme
#From Anela's paper- original
#E(net)$width <- 2+E(net)$weight/4
E(net)$width <- 2 + (E(net)$weight * 2)  # Multiply weight by 2 (or another factor) to amplify the interactions in a proportional way



#If you like the layout- comment these out. Otherwise this will randomly assign a layout
#l <- layout_with_graphopt(net)
#l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

#E(net)$edge.color =  adjustcolor(colorvalues[E(net)$from_group],.6)
par(mar = c(1, 0.5, 1, 1))  # Reduce margins (bottom, left, top, right)
plot.igraph(net,edge.arrow.size=0.03, edge.color=E(net)$color,
            edge.arrow.mode=2, rescale=TRUE, edge.curved=.4, layout=l, edge.lty = E(net)$lty,   # Apply linetypes
            vertex.label.color= "black",vertex.label.family="sans")  


legend("right", 
       legend = c(names(colorvalues_prey), names(linetype_map)),  # Combine colors & linetypes
       pch = c(rep(21, length(colorvalues_prey)), rep(NA, length(linetype_map))),  # Circles for color, no symbol for lines
       pt.bg = c(colorvalues_prey, rep(NA, length(linetype_map))),  # Background color for nodes
       col = c(rep("black", length(colorvalues_prey)), rep("black", length(linetype_map))),  # Line color
       lty = c(rep(NA, length(colorvalues_prey)), linetype_map),  # Linetype only for edges
       lwd = c(rep(NA, length(colorvalues_prey)), rep(2, length(linetype_map))),  # Line width for linetypes
       pt.cex = 2,  # Make the color circles bigger
       cex = 0.7, 
       bty = "n", 
       ncol = 1)  # Set to 1 column for clarity


######## For PARTIAL COLOR FIG
####### PREDATORS
colorvalues_pred <- c(
  #"Delta smelt" = "turquoise4",
  "LMB" = "dodgerblue4", "Striped bass" = "dodgerblue3", "Miss. silverside"= "dodgerblue2",
  "Channel catfish" = "deepskyblue2", "White catfish" = "deepskyblue2", "Bluegill sunfish"="deepskyblue2", "Sac. pikeminnow" ="deepskyblue2",
  "Chinook salmon" = "deepskyblue2", "Tule perch" ="deepskyblue2", "Threadfin shad" = "deepskyblue2", "Golden shiner"="deepskyblue2", "Shimofuri goby"="deepskyblue2",
  "Threespine stickleback"="deepskyblue2",
  "Humans" = "purple",
  "Dec. shrimp"= "darksalmon")

#Assign colors to cat type (if not defined, they are gray)
V(net)$color <- colorvalues_pred[V(net)$Cat]
V(net)[is.na(V(net)$color)]$color <- adjustcolor("lightgray",.6)

#########################
# Assign edge colors based on predator or prey type

# Get edge colors based on predator
edge_colors <- colorvalues_pred[E(net)$from_group] 

# Replace NAs with gray
edge_colors[is.na(edge_colors)] <- "gray"

# Assign to the edges
E(net)$color <- adjustcolor(edge_colors, 0.6) # this makes sure that lines are semi-transparent


# I also want the interaction type to be added in:
# efine a mapping for linetypes based on interaction types
linetype_map <- c("exploitation" = 1,  # solid
                  "competition" = 2,  # dashed
                  "amensalism" = 4)  # dotted

# Assign linetypes to edges
E(net)$lty <- linetype_map[E(net)$int] 


# Labels- instead of fw1... default
V(net)$label <- V(net)$Cat  # Use species type as the label
#V(net)$label <- substr(V(net)$cat, 1, 3)  # Shorten label to first 3 letters
V(net)$label.cex <- 0.9  # Reduce label size


# Need to adjust this so that the edges are more extreme
#From Anela's paper- original
#E(net)$width <- 2+E(net)$weight/4
E(net)$width <- 2 + (E(net)$weight * 2)  # Multiply weight by 2 (or another factor) to amplify the interactions in a proportional way



#If you like the layout- comment these out. Otherwise this will randomly assign a layout
#l <- layout_with_graphopt(net)
#l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

#E(net)$edge.color =  adjustcolor(colorvalues[E(net)$from_group],.6)
par(mar = c(1, 0.5, 1, 1))  # Reduce margins (bottom, left, top, right)
plot.igraph(net,edge.arrow.size=0.03, edge.color=E(net)$color,
            edge.arrow.mode=2, rescale=TRUE, edge.curved=.4, layout=l, edge.lty = E(net)$lty,   # Apply linetypes
            vertex.label.color= "black",vertex.label.family="sans")  


legend("right", 
       legend = c(names(colorvalues_pred), names(linetype_map)),  # Combine colors & linetypes
       pch = c(rep(21, length(colorvalues_pred)), rep(NA, length(linetype_map))),  # Circles for color, no symbol for lines
       pt.bg = c(colorvalues_pred, rep(NA, length(linetype_map))),  # Background color for nodes
       col = c(rep("black", length(colorvalues_pred)), rep("black", length(linetype_map))),  # Line color
       lty = c(rep(NA, length(colorvalues_pred)), linetype_map),  # Linetype only for edges
       lwd = c(rep(NA, length(colorvalues_pred)), rep(2, length(linetype_map))),  # Line width for linetypes
       pt.cex = 2,  # Make the color circles bigger
       cex = 0.7, 
       bty = "n", 
       ncol = 1)  # Set to 1 column for clarity

######## For PARTIAL COLOR FIG
####### OTHER INTERACTIONS
colorvalues_int <- c(
  #"Delta smelt" = "turquoise4", "Delta smelt ELS" = "turquoise2",
  "Miss. silverside"= "dodgerblue2", "Wakasagi"= "dodgerblue1",
  "Threadfin shad" = "deepskyblue2",
  "Humans" = "purple", "Limnoithona"= "orange", 
  "SAV"= "green4", "FAV" = "green3",
  "Overbite clam"= "hotpink3", "Nutrients" = "peru")


#Assign colors to cat type (if not defined, they are gray)
V(net)$color <- colorvalues_int[V(net)$Cat]
V(net)[is.na(V(net)$color)]$color <- adjustcolor("lightgray",.6)

#########################
# Assign edge colors based on predator or prey type

# Get edge colors based on predator
edge_colors <- colorvalues_int[E(net)$from_group] 

# Replace NAs with gray
edge_colors[is.na(edge_colors)] <- "gray"

# Assign to the edges
E(net)$color <- adjustcolor(edge_colors, 0.6) # this makes sure that lines are semi-transparent


# I also want the interaction type to be added in:
# efine a mapping for linetypes based on interaction types
linetype_map <- c("exploitation" = 1,  # solid
                  "competition" = 2,  # dashed
                  "amensalism" = 4)  # dotted

# Assign linetypes to edges
E(net)$lty <- linetype_map[E(net)$int] 


# Labels- instead of fw1... default
V(net)$label <- V(net)$Cat  # Use species type as the label
#V(net)$label <- substr(V(net)$cat, 1, 3)  # Shorten label to first 3 letters
V(net)$label.cex <- 0.9  # Reduce label size


# Need to adjust this so that the edges are more extreme
#From Anela's paper- original
#E(net)$width <- 2+E(net)$weight/4
E(net)$width <- 2 + (E(net)$weight * 2)  # Multiply weight by 2 (or another factor) to amplify the interactions in a proportional way



#If you like the layout- comment these out. Otherwise this will randomly assign a layout
#l <- layout_with_graphopt(net)
#l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)

#E(net)$edge.color =  adjustcolor(colorvalues[E(net)$from_group],.6)
par(mar = c(1, 0.5, 1, 1))  # Reduce margins (bottom, left, top, right)
plot.igraph(net,edge.arrow.size=0.03, edge.color=E(net)$color,
            edge.arrow.mode=2, rescale=TRUE, edge.curved=.4, layout=l, edge.lty = E(net)$lty,   # Apply linetypes
            vertex.label.color= "black",vertex.label.family="sans")  


legend("right", 
       legend = c(names(colorvalues_pred), names(linetype_map)),  # Combine colors & linetypes
       pch = c(rep(21, length(colorvalues_pred)), rep(NA, length(linetype_map))),  # Circles for color, no symbol for lines
       pt.bg = c(colorvalues_pred, rep(NA, length(linetype_map))),  # Background color for nodes
       col = c(rep("black", length(colorvalues_pred)), rep("black", length(linetype_map))),  # Line color
       lty = c(rep(NA, length(colorvalues_pred)), linetype_map),  # Linetype only for edges
       lwd = c(rep(NA, length(colorvalues_pred)), rep(2, length(linetype_map))),  # Line width for linetypes
       pt.cex = 2,  # Make the color circles bigger
       cex = 0.7, 
       bty = "n", 
       ncol = 1)  # Set to 1 column for clarity





### Making a reproducible figure when a specific species is selected




















######### Playing around with Maps
WW_Delta <- deltamapr::WW_Delta
WW_Watershed <- deltamapr::WW_Watershed
SFB <- WW_Watershed[c(418, 432:433),]
#SFB$HNAME <- "SFB"
SuisunB <- deltamapr::WW_Watershed[c(308:309, 320:325, 332:334, 411:412, 414:415),] #160),]
check <- deltamapr::WW_Delta[c(172),]
River <- deltamapr::WW_Watershed[c(337:338, 343,353, 367:384, 402:403, 409, 421, 425:427, 430:431, 436:437),]
Deltaw <- deltamapr::WW_Watershed[c(161:331, 438),] #c(161:163, 202, 213, 215, 237, 267, 288:289, 294:297, 311:315, 352, 438),]
#Folsom lake 434 not included right now
ggplot(WW_Delta)+
  geom_sf(aes())+
  theme_bw()

ggplot(SFB)+
  geom_sf(fill="red")+
  theme_bw()


#This gets you basic locations, whole map
ggplot()+#WW_Watershed)+
  geom_sf(data=R_Delta, fill= "#35B779FF" , alpha= 0.7)+
  geom_sf(data= R_Suisun, fill = "#31688EFF" , alpha= 0.7)+
  geom_sf(aes())+
  geom_sf(data= SFB, fill= "#440154FF", alpha= 0.7)+ #this order matters
  
  geom_sf(data= SuisunB, fill=  "#31688EFF" , alpha= 0.7)+#this order matters
  geom_sf(data= Deltaw, fill=  "#35B779FF" , alpha= 0.7)+
  geom_sf(data= River, fill= "#FDE725FF")+ # can't really see- fix and post
  geom_sf(data= check, fill=  "#31688EFF" , alpha= 0.7)+
  theme_bw()


##### That map is generally good- I want to add in CA coastline
#Options courtesy of chatgpt because other code is on my other laptop....


# Get US states geometry (scale = "medium" for decent resolution)
states <- ne_states(country = "United States of America", returnclass = "sf")

# Filter for California
california <- states %>% filter(name == "California")
oregon <- states %>% filter(name == "Oregon")
nevada <- states %>% filter(name == "Nevada")
idaho <- states %>% filter(name == "Idaho")


# "Zoomed" plot show highlighted areas
ggplot()+
  geom_sf(data = california, fill = "gray85", color = "black") +  # coastline/land base
  geom_sf(data=R_Delta, fill= "#5DC863FF", color="#5DC863FF", alpha= 0.7)+
  geom_sf(data= R_Suisun, fill = "#21908CFF", alpha= 0.7)+
  geom_sf(aes())+
  geom_sf(data= SFB, fill= "#3B528BFF", alpha= 0.7)+ #this order matters
  
  geom_sf(data= SuisunB, fill=  "#21908CFF", alpha= 0.7)+#this order matters
  geom_sf(data= Deltaw, fill=  "#5DC863FF" , alpha= 0.7)+
  #geom_sf(data= River, fill= "#FDE725FF", color= "#FDE725FF")+
  geom_sf(data= River, fill= "orange", color= "orange")+
  geom_sf(data= check, fill=  "#21908CFF" , alpha= 0.7)+# can't really see- fix and post
  coord_sf(
    xlim = c(-123.1, -120.7),
    ylim = c(37.2, 40.2),
    expand = FALSE
  )+
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "#dbe9f6"),  # light ocean blue
    panel.grid = element_line(color = NA)
  )


# "Zoomed" plot colorless
ggplot()+
  geom_sf(data = california, fill = "gray85", color = "black") +  # coastline/land base
  geom_sf(data=R_Delta)+#, fill= "#5DC863FF", color="#5DC863FF", alpha= 0.7)+
  geom_sf(data= R_Suisun)+ #, fill = "#31688EFF", alpha= 0.7)+
  geom_sf(aes())+
  geom_sf(data= SFB)+#, fill= "#440154FF", alpha= 0.7)+ #this order matters
  
  geom_sf(data= SuisunB)+#, fill=  "#31688EFF", alpha= 0.7)+#this order matters
  geom_sf(data= Deltaw)+#, fill=  "#5DC863FF", alpha= 0.7)+
  #geom_sf(data= River, fill= "#FDE725FF", color= "#FDE725FF")+
  geom_sf(data= River, fill= "orange", color= "orange")+
  geom_sf(data= check)+ #, fill=  "#31688EFF" , alpha= 0.7)+# can't really see- fix and post
  coord_sf(
    xlim = c(-123.1, -120.7),
    ylim = c(37.2, 40.2),
    expand = FALSE
  )+
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "#dbe9f6"),  # light ocean blue
    panel.grid = element_line(color = NA)
  )


# "Zoomed" plot show highlighted areas one at a time
ggplot()+
  geom_sf(data = california, fill = "gray85", color = "black") +  # coastline/land base
  geom_sf(data=R_Delta)+#, fill= "#35B779FF", color="#35B779FF", alpha= 0.7)+
  geom_sf(data= R_Suisun)+#, fill = "#21908CFF", alpha= 0.7)+
  geom_sf(aes())+
  geom_sf(data= SFB)+#, fill= "#3B528BFF", alpha= 0.7)+ #this order matters
  
  geom_sf(data= SuisunB)+#, fill=  "#21908CFF", alpha= 0.7)+#this order matters
  geom_sf(data= Deltaw)+ #, fill=  "#35B779FF" , alpha= 0.7)+
  #geom_sf(data= River, fill= "#FDE725FF", color= "#FDE725FF")+
  geom_sf(data= River)+ #, fill= "orange", color= "orange")+
  geom_sf(data= check)+#, fill=  "#21908CFF" , alpha= 0.7)+# can't really see- fix and post
  coord_sf(
    xlim = c(-123.1, -120.7),
    ylim = c(37.2, 40.2),
    expand = FALSE
  )+
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "#dbe9f6"),  # light ocean blue
    panel.grid = element_line(color = NA)
  )


# Big map of almost whole state (staticMap in code)
ggplot()+
  geom_sf(data = california, fill = "gray85", color = "black") +# coastline/land base
  geom_sf(data = oregon, fill = "gray85", color = "black") +
  geom_sf(data = nevada, fill = "gray85", color = "black") +
  geom_sf(data = idaho, fill = "gray85", color = "black") +
  geom_sf(data=R_Delta, fill= "#35B779FF" , alpha= 0.7)+
  geom_sf(data= R_Suisun, fill = "#31688EFF" , alpha= 0.7)+
  geom_sf(aes())+
  geom_sf(data= SFB, fill= "#440154FF", alpha= 0.7)+ #this order matters
  
  geom_sf(data= SuisunB, fill=  "#31688EFF" , alpha= 0.7)+#this order matters
  geom_sf(data= Deltaw, fill=  "#35B779FF" , alpha= 0.7)+
  #geom_sf(data= River, fill= "#FDE725FF")+ # can't really see- fix and post
  geom_sf(data= River, fill= "orange", color= "orange")+
  geom_sf(data= check, fill=  "#31688EFF" , alpha= 0.7)+
  geom_rect(aes(xmin= -123.1, xmax=-120.7, ymin= 37.2, ymax=40.2), color= "black", alpha= 0)+
  #geom_sf(aes(fill= EcoZone))+
  coord_sf(
    xlim = c(-125, -115),
    ylim = c(32.7, 42.1),
    expand = FALSE
  )+
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "#dbe9f6"),  # light ocean blue
    panel.grid = element_line(color = NA)
  )


#"#440154FF" "#3B528BFF" "#21908CFF" "#5DC863FF" "#FDE725FF"
# Big map of almost whole state (staticMap in code)
ggplot()+
  geom_sf(data = california, fill = "gray85", color = "black") +# coastline/land base
  geom_sf(data = oregon, fill = "gray85", color = "black") +
  geom_sf(data = nevada, fill = "gray85", color = "black") +
  geom_sf(data = idaho, fill = "gray85", color = "black") +
  geom_sf(data=R_Delta, fill= "#5DC863FF" , alpha= 0.7)+
  geom_sf(data= R_Suisun, fill = "#21908CFF" , alpha= 0.7)+
  geom_sf(aes())+
  geom_sf(data= SFB, fill= "#3B528BFF", alpha= 0.7)+ #this order matters
  
  geom_sf(data= SuisunB, fill=  "#21908CFF" , alpha= 0.7)+#this order matters
  geom_sf(data= Deltaw, fill=  "#5DC863FF" , alpha= 0.7)+
  #geom_sf(data= River, fill= "#FDE725FF")+ # can't really see- fix and post
  geom_sf(data= River, fill= "orange", color= "orange")+
  geom_sf(data= check, fill=  "#21908CFF" , alpha= 0.7)+
  geom_rect(aes(xmin= -123.1, xmax=-120.7, ymin= 37.2, ymax=40.2), color= "black", alpha= 0)+
  #geom_sf(aes(fill= EcoZone))+
  coord_sf(
    xlim = c(-125, -115),
    ylim = c(32.7, 42.1),
    expand = FALSE
  )+
  theme_bw()+
  theme(
    panel.background = element_rect(fill = "#dbe9f6"),  # light ocean blue
    panel.grid = element_line(color = NA)
  )


#Know what colors are from alluvial plot: viridis(11)
#"#440154FF" "#482576FF" "#414487FF" "#35608DFF" "#2A788EFF" "#21908CFF" "#22A884FF" "#43BF71FF" "#7AD151FF" "#BBDF27FF" "#FDE725FF"
# SFB: "#2A788EFF"
#Suisun: "#22A884FF"
# Delta: "#7AD151FF"
# Rivers: "#FDE725FF"

#Could also shorten it to just 4 locations for presentation:
#SFB:"#440154FF" 
#Suisun: "#31688EFF" 
#Delta: "#35B779FF" 
#Rivers: "#FDE725FF"


















##### Playing around with a chord diagram: Chat GPT code
# 
# FW <- read.delim("Mini_SppInt_DF.txt", header = TRUE, sep = "\t", quote = "\"", 
#                  dec = ".", fill = TRUE, comment.char = "",
#                  stringsAsFactors=FALSE)
# 
# # FW$Sp1.Common <- as.factor(FW$Sp1.Common)
# # FW$Sp2.Common <- as.factor(FW$Sp2.Common)
# 
# # Create a new data frame with frequency of each species combination with common names
# comb_freq <- FW %>%
#   group_by(Sp1.Common, Sp2.Common) %>%
#   summarise(frequency = n(), .groups = 'drop')
# 
# # Specify connections (from, to) and values
# connections <- data.frame(
#   from =comb_freq$Sp1.Common,
#   to = comb_freq$Sp2.Common,
#   value = comb_freq$frequency
# )
# 
# # Create the chord diagram colored by categories
# chordDiagram(
#   connections,
#   #grid.col = species_colors,
#   transparency = 0.5,
#   directional = TRUE,
#   annotationTrack = c("grid", "name"),
#   preAllocateTracks = 1
# )
# 
# 
# # Create a new data frame with frequency of each species combination with cateogry
# comb_freq <- FW %>%
#   group_by(Sp1.Cat, Sp2.Cat) %>%
#   summarise(frequency = n(), .groups = 'drop')
# 
# # Specify connections (from, to) and values
# connections <- data.frame(
#   from =comb_freq$Sp1.Cat,
#   to = comb_freq$Sp2.Cat,
#   value = comb_freq$frequency
# )



# #### DOI chat gpt doesn't really work
# # Create the chord diagram
# chordDiagram(
#   connections,
#   transparency = 0.5,
#   directional = TRUE,
#   annotationTrack = c("grid"),
#   preAllocateTracks = 1
# )
# 
# # Add labels that are perpendicular to the circle and near the chords
# circos.track(ylim = c(0, 1), track.index = 1, panel.fun = function(x, y) {
#   # Get unique species labels
#   species_labels <- unique(c(connections$from, connections$to))
#   
#   # Loop through each unique label to position it
#   for (i in seq_along(species_labels)) {
#     # Get the angle for the current label
#     angle <- get.angle(species_labels[i])  # Updated to get the angle correctly
#     # Calculate the position for each label
#     x_pos <- cos(angle) * 0.8  # Position slightly outside the circle
#     y_pos <- sin(angle) * 0.8
#     
#     # Place the label with proper orientation
#     text(x = x_pos, y = y_pos, 
#          labels = species_labels[i], 
#          srt = angle * 180 / pi + 90,  # Perpendicular to the circle
#          adj = c(0.5, 0.5), 
#          cex = 0.8)
#   }
# }, bg.border = NA)
# 
# # Function to get the angle based on the label
# get.angle <- function(label) {
#   angle <- ifelse(label %in% connections$from, 
#                   which(unique(connections$from) == label) * 360 / length(unique(c(connections$from, connections$to))),
#                   which(unique(connections$to) == label) * 360 / length(unique(c(connections$from, connections$to))) + 180)
#   return(angle * pi / 180)  # Convert to radians
# }



# Another try

#install.packages("circlize")
library(circlize)

# # Sample data
# data <- data.frame(
#   from = c("Species A", "Species B", "Species C", "Species D"),
#   to = c("Species B", "Species C", "Species A", "Species A"),
#   value = c(10, 5, 7, 3)
# )
# 
# # Define categories (genera)
# category <- c("Genus 1", "Genus 1", "Genus 2", "Genus 2")
# 
# # Define colors for each genus
# grid_colors <- c("Genus 1" = "blue", "Genus 2" = "green")

# Create the chord diagram
chordDiagram(
  x = connections, 
  annotationTrack = "grid", 
  preAllocateTracks = list(
    track.height = 0.1
  )#,
  #grid.col = grid_colors
)

# Add category labels (optional)
circos.trackPlotRegion(
  track.index = 1, 
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter, 
      CELL_META$ylim[1] + mm_y(15), 
      CELL_META$sector.index, 
      facing = "clockwise", 
      niceFacing = TRUE
    )
  },
  bg.border = NA
)





##### Playing around with a chord diagram: Chat GPT code

# Note that this file should be updated/pulled from the Spp_Aug24_Dataplay sheet in spp int file

FW <- read.delim("Spp_int_DF_Feb25.txt", header = TRUE, sep = "\t", quote = "\"", 
                 dec = ".", fill = TRUE, comment.char = "",
                 stringsAsFactors=FALSE)

# Only food web interactions

fw <- as.data.frame(FW[which(FW$Int.type == "food web" |FW$Int.type == "food web, contaminants"
                             |FW$Int.type == "food web, habitat"
                             |FW$Int.type == "food web; habitat"),])

# Important that you separate so it's not pulling a bunch of records from the same paper. This should give distinct spp in with # of studies showing that interaction
fw <- fw %>% 
  filter(!is.na(Sp1.Cat) & !is.na(Sp2.Cat) & Sp1.Cat != "" & Sp2.Cat != "") 

fw$Sp1.Cat[which(fw$Sp1.Cat == "Ciliate")] <- "Microplankton"
fw$Sp1.Cat[which(fw$Sp1.Cat == "Pinniped")] <- "Marine mammal" #keep these separate for later, but now combine
fw$Sp1.Cat[which(fw$Sp1.Cat == "Trematode")] <- "Flatworm"
fw$Sp1.Cat[which(fw$Sp1.Cat == "Human")] <- "Humans"
fw$Sp1.Cat[which(fw$Sp1.Cat == "Anthozoan")] <- "Anemone"
fw$Sp1.Cat[which(fw$Sp1.Cat == "Squid")] <- "Cephalopod"
fw$Sp1.Cat[which(fw$Sp1.Cat == "Dinoflagellate")] <- "Dinoflagellates"
fw$Sp1.Cat[which(fw$Sp1.Cat == "Plants")] <- "Plant"
fw$Sp1.Cat[which(fw$Sp1.Cat == "Nemertea")] <- "Nemertean"
fw$Sp1.Cat[which(fw$Sp1.Cat == "Worm")] <- "Annelid"
fw$Sp1.Cat[which(fw$Sp1.Cat == "Insects")] <- "Insect"

fw$Sp2.Cat[which(fw$Sp2.Cat == "Ciliate")] <- "Microplankton"
fw$Sp2.Cat[which(fw$Sp2.Cat == "Pinniped")] <- "Marine mammal" #keep these separate for later, but now combine
fw$Sp2.Cat[which(fw$Sp2.Cat == "Trematode")] <- "Flatworm"
fw$Sp2.Cat[which(fw$Sp2.Cat == "Human")] <- "Humans"
fw$Sp2.Cat[which(fw$Sp2.Cat == "Anthozoan")] <- "Anemone"
fw$Sp2.Cat[which(fw$Sp2.Cat == "Squid")] <- "Cephalopod"
fw$Sp2.Cat[which(fw$Sp2.Cat == "Dinoflagellate")] <- "Dinoflagellates"
fw$Sp2.Cat[which(fw$Sp2.Cat == "Plants")] <- "Plant"
fw$Sp2.Cat[which(fw$Sp2.Cat == "Nemertea")] <- "Nemertean"
fw$Sp2.Cat[which(fw$Sp2.Cat == "Worm")] <- "Annelid"
fw$Sp2.Cat[which(fw$Sp2.Cat == "Insects")] <- "Insect"


# fw2 <- fw%>% Not necessary because not looking at total studies, looking at 
#   group_by(Title, Sp1.Common, Sp1.Cat, Sp2.Common, Sp2.Cat, Int) %>% 
#   distinct(Title, Sp1.Common, Sp1.Cat, Sp2.Common, Sp2.Cat, Int, .keep_all = TRUE) 

#This was to test out the code. Ignore
# Create a new data frame with frequency of each species combination with category
# comb_freq <- FW %>%
#   filter(!is.na(Sp1.Cat) & !is.na(Sp2.Cat) & Sp1.Cat != "" & Sp2.Cat != "") %>%
#   group_by(Sp1.Cat, Sp2.Cat) %>%
#   summarise(frequency = n(), .groups = 'drop')
# 
# comb_freq$Sp1.Cat[which(comb_freq$Sp1.Cat == "FIsh")] <- "Fish" #quick fix, shouldn't need to do this after file update 9/26
# 
# # Specify connections (from, to) and values
# connections <- data.frame(
#   from =comb_freq$Sp1.Cat,
#   to = comb_freq$Sp2.Cat,
#   value = comb_freq$frequency
# )
# 
# 
# # Create the chord diagram
# chordDiagram(
#   x = connections, 
#   annotationTrack = "grid", 
#   preAllocateTracks = list(
#     track.height = 0.3
#   )#,
#   #grid.col = grid_colors
# )
# 
# # Add category labels (optional)
# circos.trackPlotRegion(
#   track.index = 1, 
#   panel.fun = function(x, y) {
#     circos.text(
#       CELL_META$xcenter, 
#       CELL_META$ylim[1] + mm_y(10), 
#       CELL_META$sector.index, 
#       facing = "clockwise", 
#       niceFacing = TRUE
#     )
#   },
#   bg.border = NA
# )




# Now, split into regions

all <- as.data.frame(fw[which(fw$Bay.Delta2 == "All" |fw$Bay.Delta2 == "All + Pacific" ),]) #skipped feb 25

# Create a new data frame with frequency of each species combination with category
all_freq <- all %>%
  group_by(Sp1.Cat, Sp2.Cat) %>%
  summarise(frequency = n(), .groups = 'drop')

# Specify connections (from, to) and values
connections <- data.frame(
  from =all_freq$Sp1.Cat,
  to = all_freq$Sp2.Cat,
  value = all_freq$frequency
)



# Create the chord diagram
#par(bg="#003E51") will change plot bg to the blue USBR ppt
# add col = "white"  # Set text color to white
par(bg = "white")

chordDiagram(
  x = connections, 
  annotationTrack = "grid", 
  preAllocateTracks = list(
    track.height = 0.2 # adjust track height to decrease circle to see more text labels
  )#,
  #grid.col = grid_colors
)

# Add category labels (optional)
circos.trackPlotRegion(
  track.index = 1, 
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter, 
      CELL_META$ylim[1] + mm_y(10), 
      CELL_META$sector.index, 
      facing = "clockwise", 
      niceFacing = TRUE
    )
  },
  bg.border = NA
)


#Assign colors by category:
grid_colors <- c(
  "Amphibian" = "limegreen",
  "Amphipod" = "tomato4",
  "Annelid" = "lightsalmon",
  "Arachnid" = "red4",
  "Bacteria" = "grey",
  "Bird" = "magenta",
  "Bivalve" = "hotpink3",
  "Copepod" = "darkorange",
  "Crustacean" = "goldenrod2",
  "Elasmobranch" = "navyblue",
  "Fish" = "dodgerblue2",
  "Flatworm" = "peru",
  "Gastropod" = "cadetblue",
  "Humans" = "purple",
  "Hydroid"= "yellow",
  "Insect" = "red",
  "Mammal" = "darkorchid4",
  "Marine mammal" = "slateblue1",
  "Microplankton"= "yellowgreen",
  "Nanoplankton" = "yellow3",
  "Nematode" = "salmon4",
  "Nudibranch" = "slategray",
  "Parasite" = "black",
  "Phytoplankton" = "darkolivegreen1",
  "Plant" = "darkgreen",
  "Polychaete" = "salmon",
  "Reptile" = "darkolivegreen4",
  "Rotifer" = "rosybrown2",
  "Zooplankton" = "aquamarine",
  "Detritus" = "burlywood4"
  
)


sfb <- as.data.frame(fw[which(fw$Loc == "SFB" |fw$Loc == "Pacific + SFB"
                              | fw$Loc == "SFB + Suisun" | fw$Loc == "SFB + Suisun + Delta" | fw$Bay.Delta2 == "SFB + Delta"
                              | fw$Loc == "SFB + Rivers"| fw$Loc == "SFB + Delta + Rivers"),]) 
#1024 obs

# Create a new data frame with frequency of each species combination with category
sfb_freq <- sfb %>%
  group_by(Sp1.Cat, Sp2.Cat) %>%
  summarise(frequency = n(), .groups = 'drop')

# Specify connections (from, to) and values
connections <- data.frame(
  from =sfb_freq$Sp1.Cat,
  to = sfb_freq$Sp2.Cat,
  value = sfb_freq$frequency
)

#sfb_cat <- unique(connections$from)

# Create the chord diagram
chordDiagram(
  x = connections, 
  annotationTrack = "grid", 
  preAllocateTracks = list(
    track.height = 0.3
  ),
  grid.col = grid_colors
)

# Add category labels (optional)
circos.trackPlotRegion(
  track.index = 1, 
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter, 
      CELL_META$ylim[1] + mm_y(10), 
      CELL_META$sector.index, 
      facing = "clockwise", 
      niceFacing = TRUE
    )
  },
  bg.border = NA
)





sui <- as.data.frame(fw[which(fw$Loc == "Suisun" |fw$Loc == "Suisun + Delta" |fw$Loc == "SFB + Suisun"
                              |fw$Loc == "SFB + Suisun + Delta" |fw$Loc == "Delta + Suisun + Rivers"
                              |fw$Loc == "Suisun + Delta + Rivers"),])

# Create a new data frame with frequency of each species combination with category
sui_freq <- sui %>%
  group_by(Sp1.Cat, Sp2.Cat) %>%
  summarise(frequency = n(), .groups = 'drop')

# Specify connections (from, to) and values
connections <- data.frame(
  from =sui_freq$Sp1.Cat,
  to = sui_freq$Sp2.Cat,
  value = sui_freq$frequency
)



# Create the chord diagram
chordDiagram(
  x = connections, 
  annotationTrack = "grid", 
  preAllocateTracks = list(
    track.height = 0.3
  ),
  grid.col = grid_colors
)

# Add category labels (optional)
circos.trackPlotRegion(
  track.index = 1, 
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter, 
      CELL_META$ylim[1] + mm_y(10), 
      CELL_META$sector.index, 
      facing = "clockwise", 
      niceFacing = TRUE
    )
  },
  bg.border = NA
)


delta <- as.data.frame(fw[which(fw$Loc == "Delta" | fw$Loc == "Suisun + Delta"| fw$Loc == "SFB + Suisun + Delta"
                                | fw$Loc == "Delta + Suisun + Rivers" | fw$Loc == "Delta + Rivers" | 
                                  fw$Loc =="SFB + Delta + Rivers"),])

# Create a new data frame with frequency of each species combination with category
d_freq <- delta %>%
  group_by(Sp1.Cat, Sp2.Cat) %>%
  summarise(frequency = n(), .groups = 'drop')

# Specify connections (from, to) and values
connections <- data.frame(
  from =d_freq$Sp1.Cat,
  to = d_freq$Sp2.Cat,
  value = d_freq$frequency
)



# Create the chord diagram
chordDiagram(
  x = connections, 
  annotationTrack = "grid", 
  preAllocateTracks = list(
    track.height = 0.3
  ),
  grid.col = grid_colors
)

# Add category labels (optional)
circos.trackPlotRegion(
  track.index = 1, 
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter, 
      CELL_META$ylim[1] + mm_y(10), 
      CELL_META$sector.index, 
      facing = "clockwise", 
      niceFacing = TRUE
    )
  },
  bg.border = NA
)




riv <- as.data.frame(fw[which(fw$Loc == "Delta + Rivers" | fw$Loc == "Rivers" | fw$Loc == "Rivers + SFB"
                              |fw$Loc == "Delta + Suisun + Rivers" |fw$Loc == "Suisun + Delta + Rivers" |
                                fw$Loc == "Suisun; Rivers" | fw$Loc == "SFB + Delta + Rivers" |
                                fw$Loc == "SFB + Rivers"),])

# Create a new data frame with frequency of each species combination with category
riv_freq <- riv %>%
  group_by(Sp1.Cat, Sp2.Cat) %>%
  summarise(frequency = n(), .groups = 'drop')

# Specify connections (from, to) and values
connections <- data.frame(
  from =riv_freq$Sp1.Cat,
  to = riv_freq$Sp2.Cat,
  value = riv_freq$frequency
)



# Create the chord diagram
chordDiagram(
  x = connections, 
  annotationTrack = "grid", 
  preAllocateTracks = list(
    track.height = 0.3
  ),
  grid.col = grid_colors
)

# Add category labels (optional)
circos.trackPlotRegion(
  track.index = 1, 
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter, 
      CELL_META$ylim[1] + mm_y(10), 
      CELL_META$sector.index, 
      facing = "clockwise", 
      niceFacing = TRUE
    )
  },
  bg.border = NA
)


#### Now do combined- all food web interactions, but with species

# Create a new data frame with frequency of each species combination with cateogry
comb_freq <- fw %>%
  group_by(Sp1.Common, Sp2.Common) %>%
  summarise(frequency = n(), .groups = 'drop')

# Specify connections (from, to) and values
connections <- data.frame(
  from =comb_freq$Sp1.Common,
  to = comb_freq$Sp2.Common,
  value = comb_freq$frequency
)

# connections <- c(connections[2:315,], connections[317:682,], connections[684:963,],
#                  connections[965:1137,], connections[]

# Create the chord diagram
chordDiagram(
  x = connections, 
  annotationTrack = "grid", 
  preAllocateTracks = list(
    track.height = 0.2
  )#,
  #grid.col = grid_colors
)

# Add category labels (optional)
circos.trackPlotRegion(
  track.index = 1, 
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter, 
      CELL_META$ylim[1] + mm_y(10), 
      CELL_META$sector.index, 
      facing = "clockwise", 
      niceFacing = TRUE
    )
  },
  bg.border = NA
)
















































# Create the chord diagram colored by categories
chordDiagram(
  connections,
  #grid.col = species_colors,
  transparency = 0.5,
  directional = TRUE,
  annotationTrack = c("grid", "name"),
  preAllocateTracks = 1
)

chordDiagram(connections, annotationTrack = "grid", preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)



# Define colors for categories and species variations
category_colors <- c("Category 1" = "#C2E0E8",  # Light blue
                     "Category 2" = "#A3D99D",  # Light green
                     "Category 3" = "#FFB1B1")  # Light coral

# Define varying shades for species within each category
species_colors <- c("Species A" = "#91C6DA",  # Dark blue
                    "Species B" = "#A4D1E7",  # Medium blue
                    "Species C" = "#B8E3B4",  # Dark green
                    "Species D" = "#A3D99D",  # Light green 
                    "Species E" = "#F5B7B1")  # Dark coral



# Clear the plot to add the legend later
circos.clear()

# Create a legend
legend_labels <- names(category_colors)
legend_colors <- category_colors

# Plotting the legend outside the plot
par(xpd = TRUE)  # Allow drawing outside of plot area
legend("topright", 
       legend = legend_labels, 
       fill = legend_colors, 
       title = "Categories", 
       bty = "n", 
       cex = 1.2)
par(xpd = FALSE)  # Reset

#### From Chat GPT
# Install and load the necessary package
install.packages("circlize")
library(circlize)

# Define species and their taxonomic categories
species <- c("Species A", "Species B", "Species C", "Species D", "Species E")
categories <- c("Category 1", "Category 1", "Category 2", "Category 2", "Category 3")

# Specify connections (from, to) and values
connections <- data.frame(
  from = c("Species A", "Species A", "Species B", "Species C", "Species D", "Species E"),
  to = c("Species B", "Species C", "Species D", "Species A", "Species C", "Species E"),
  value = c(10, 5, 15, 10, 5, 8)
)

# Define colors for categories and species variations
category_colors <- c("Category 1" = "#C2E0E8",  # Light blue
                     "Category 2" = "#A3D99D",  # Light green
                     "Category 3" = "#FFB1B1")  # Light coral

# Define varying shades for species within each category
species_colors <- c("Species A" = "#91C6DA",  # Dark blue
                    "Species B" = "#A4D1E7",  # Medium blue
                    "Species C" = "#B8E3B4",  # Dark green
                    "Species D" = "#A3D99D",  # Light green 
                    "Species E" = "#F5B7B1")  # Dark coral

# Create the chord diagram colored by categories
chordDiagram(
  connections,
  grid.col = species_colors,
  transparency = 0.5,
  directional = TRUE,
  annotationTrack = c("grid", "name"),
  preAllocateTracks = 1
)

# Clear the plot to add the legend later
circos.clear()

# Create a legend
legend_labels <- names(category_colors)
legend_colors <- category_colors

# Plotting the legend outside the plot
par(xpd = TRUE)  # Allow drawing outside of plot area
legend("topright", 
       legend = legend_labels, 
       fill = legend_colors, 
       title = "Categories", 
       bty = "n", 
       cex = 1.2)
par(xpd = FALSE)  # Reset




# Another version- comes up with error code
library(networkD3)

# Define the connections
links <- data.frame(
  source = c(0, 0, 1, 2, 3),
  target = c(1, 2, 3, 0, 4),
  value = c(10, 5, 15, 10, 8)
)

# Define node names
nodes <- data.frame(name = c("Species A", "Species B", "Species C", "Species D", "Species E"))

# Create the chord diagram
chordNetwork(links, nodes)


library(emep)
library(ggplot2)
library(data.table)
data(emep)
setDT(emep)

emep$Value <- as.numeric(emep$Value)
class(emep$Value)
emep$value2 <- ifelse(emep$Unit == "mg/GJ", 
                      emep$Value/1000, 
                      emep$Value)
emep[Unit == "mg/GJ", 
     value2 := Value/1000]

emep[Unit == "??g/GJ", 
     value2 := Value/1000/1000]

emep[Unit == "??g/GJ", 
     value2 := Value/1000/1000]


unique(emep$NFR)

emep[NFR == "1.A.1.a" &
       Fuel == "Hard Coal"]

emep[NFR == "1.A.1.a" &
       Unit %in% c("mg/GJ", "g/GJ", "??g/GJ"),
     unique(Unit)] # todo en g/GJ

fs[order(fs)]

emep[NFR == "1.A.1.a" &
       Unit %in% c("mg/GJ", "g/GJ", "??g/GJ"),
     unique(Fuel, na.rm = T)] -> fs

emep$Fuel2 <- ifelse(
  emep$Fuel %in% c("Brown Coal/Lignite",
                   "Coking Coal, Steam Coal & Sub-Bituminous Coal"),
  "Brown Coal",
  ifelse(
    emep$Fuel %in% c("Gas oil", "Gas Oil"),
    "Light oil",
    ifelse(
      emep$Fuel %in% c( "Wood and wood waste (clean wood waste)"),
      "Biomass",
      ifelse(
        emep$Fuel %in% "Residual Oil",
        "Heavy Fuel Oil",
        ifelse(
          emep$Fuel %in% c("Gaseous fuels", 
                           "Natural Gas",
                           "Natural gas"),
          "Gaseous Fuels",
          emep$Fuel)))))

emep[NFR == "1.A.1.a" &
       Unit %in% c("mg/GJ", "g/GJ", "??g/GJ"),
     unique(Fuel2, na.rm = T)] -> fs2

fs2

emep[NFR == "1.A.1.a" &
       Unit %in% c("mg/GJ", "g/GJ", "??g/GJ"),
     mean(value2, na.rm = T),
     by = .(Pollutant, Fuel2)] -> xx


ggplot(xx[Pollutant %in% c("PM10", "NOx", "SOx", "CO", "NMVOC")], 
       aes(x = Pollutant,
           y = V1)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~Fuel2,
             scales = "free_x") +
  coord_flip()


ggplot(xx[Pollutant %in% c("PM10", "NOx", "SOx", "CO", "NMVOC")], 
       aes(x = Pollutant,
           y = V1,
           fill = Fuel2)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  facet_wrap(.
             ~Pollutant,
             nrow = 1,
             scales = "free")+
  scale_fill_brewer("",
                    type = "qual", 
                    palette = 2) +
  labs(x = NULL,
       y = "g/GJ")+
  theme(text = element_text(size = 16)) -> p
png(filename = "plot1_WMO.png", 
    width = 3000, height = 1500, res = 300, units = "px")
print(p)
dev.off()

# 1.A.B

emep[NFR == "1.A.1.b" &
       Unit %in% c("mg/GJ", "g/GJ", "??g/GJ"),
     unique(Unit)] # todo en g/GJ

emep[NFR == "1.A.1.b" &
       Unit %in% c("mg/GJ", "g/GJ", "??g/GJ"),
     mean(value2, na.rm = T),
     by = .(Pollutant, Fuel2)] -> xx


ggplot(xx[Pollutant %in% c("PM10", "NOx", "SOx", "CO", "NMVOC")], 
       aes(x = Pollutant,
           y = V1)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~Fuel2,
             scales = "free_x") +
  coord_flip()

# 1.A.2 ####
sectors <- c("1.A.2.a",
             "1.A.2.b",
             "1.A.2.c",
             "1.A.2.d",
             "1.A.2.e",
             "1.A.2.f")

emep[NFR %in% sectors,
     unique(Sector)]

emep[NFR %in% sectors &
       Unit %in% c("mg/GJ", "g/GJ", "??g/GJ"),
     mean(value2, na.rm = T),
     by = .(Pollutant, Fuel2, Sector)] -> xx


ggplot(xx[Pollutant %in% c("PM10", "NOx", "SOx", "CO", "NMVOC")], 
       aes(x = Pollutant,
           y = V1,
           fill = Fuel2)) +
  geom_bar(stat = "identity") +
  facet_grid(.~Sector,
             scales = "free_x") +
  coord_flip()


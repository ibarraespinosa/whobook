#remotes::install_github("ibarraespinosa/emep")
library(data.table)
library(emep)
library(ggplot2)
load("sysdata.rda")
emep <- as.data.table(sysdata)
rm(sysdata)
names(emep)

emep$Value <- as.numeric(emep$Value)


emep[NFR == "1.A.4.b.i", unique(Sector)]
sectores <- c("1.A.4.b.i",
              "1.A.4.a.i",
              "1.A.4.c.i",
              "1.A.5.a")

pols <- c("PM10",
          "NOx",
          "NMVOC",
          "NH3")
emep[NFR %in% sectores, unique(Sector)]

emep[NFR %in% sectores &
       Unit == "g/GJ", 
     unique(Pollutant)]

emep[NFR %in% sectores &
       Unit == "g/GJ",
     mean(Value, na.rm = T),
     by = .(Fuel, Pollutant)] -> xx

unique(xx$Fuel)

emep$Fuel <- gsub(pattern = "Wood and similar wood waste" , replacement = 
                  "Wood",
                x = emep$Fuel)


emep$Fuel <- ifelse(emep$Fuel %in% c("Fuel oil (Residual fuel oil)", 
                                 "Fuel oil (Distillate fuel oil)",
                                 "'Other' Liquid Fuels",
                                 "Liguid Fuels"),
                  "Liquid Fuels",
                  emep$Fuel)



emep[NFR %in% sectores &
       Unit == "g/GJ",
     mean(Value, na.rm = T),
     by = .(Fuel, Pollutant)] -> xx

unique(xx$Fuel)


split(xx, xx$Pollutant)

ggplot(xx, aes(x = Pollutant,
               y = V1)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Fuel)

ggplot(xx[Pollutant != "Indeno(1,2,3-cd)pyrene"],
       aes(x = Pollutant,
           y = V1,
           fill = Fuel)) +
  labs(y = "g/GJ", 
       x = NULL)+
  geom_bar(stat = "identity", col = "black") +
  theme_bw()+
  theme(text = element_text(size = 16)) -> p

p

png("chapter3_small.png", width = 3000, height = 1000, res = 300)
print(p)
dev.off()

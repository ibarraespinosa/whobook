#remotes::install_github("ibarraespinosa/emep")
library(data.table)
library(emep)
library(ggplot2)
load("sysdata.rda")
emep <- as.data.table(sysdata)
rm(sysdata)
names(emep)

emep$Value <- as.numeric(emep$Value)


sectores <- c("1.A.4.a.ii",
              "1.A.4.b.ii",
              "1.A.4.c.ii",
              "1.A.5.b")

pols <- c("PM10",
          "NOx",
          "NMVOC",
          "NH3")
emep[NFR %in% sectores, unique(Sector)]

emep[NFR %in% sectores]


emep[NFR %in% sectores &
       Unit == "g/tonnes fuel", 
     unique(Pollutant)]

emep[NFR %in% sectores &
       Unit == "g/tonnes fuel",
     mean(Value, na.rm = T),
     by = .(Fuel, Pollutant)] -> xx

unique(xx$Fuel)


emep[NFR %in% sectores &
       Unit == "g/tonnes fuel",
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
  labs(y = "g/tonnes fuel", 
       x = NULL)+
  scale_y_log10()+
  geom_bar(stat = "identity", col = "black") +
  theme_bw()+
  theme(text = element_text(size = 16)) -> p

p

png("chapter3_small.png", width = 3000, height = 1000, res = 300)
print(p)
dev.off()

#remotes::install_github("ibarraespinosa/emep")
library(data.table)
library(emep)
library(ggplot2)
load("sysdata.rda")
emep <- as.data.table(sysdata)
rm(sysdata)
names(emep)

emep$Value <- as.numeric(emep$Value)

sectores <- c("1.A.2.a",
              "1.A.2.b",
              "1.A.2.c",
              "1.A.2.d",
              "1.A.2.e",
              "1.A.2.f")

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

xx$Fuel <- gsub(pattern = "'Other' Liquid Fuels", replacement = 
                  "Liguid Fuels",
                x = xx$Fuel)

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
  geom_bar(stat = "identity", col = "black", position = "dodge") +
  theme_bw()+
  theme(text = element_text(size = 16)) -> p

png("chapter3_fig2.png", width = 3000, height = 1000, res = 300)
print(p)
dev.off()

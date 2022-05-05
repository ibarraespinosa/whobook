library(vein)
library(sf)
library(cptcity)
library(magick)
data(net)
data(profiles)
data(fe2015)
data(fkm)

PC_G <- c(
  33491, 22340, 24818, 31808, 46458, 28574, 24856, 28972, 37818, 49050, 87923,
  133833, 138441, 142682, 171029, 151048, 115228, 98664, 126444, 101027,
  84771, 55864, 36306, 21079, 20138, 17439, 7854, 2215, 656, 1262, 476, 512,
  1181, 4991, 3711, 5653, 7039, 5839, 4257, 3824, 3068
)
pc1 <- my_age(x = net$ldv, y = PC_G, name = "PC")

ef <- ef_cetesb(p = "CO", veh = "PC_FG")

array_x <- emis(
  veh = pc1,
  lkm = net$lkm,
  ef = ef,
  profile = profiles$PC_JUNE_2012$Monday,
  fortran = TRUE,
  nt = check_nt() / 2,
  simplify = TRUE,
  verbose = T
)

x_STREETS <- emis_post(
  arra = array_x,
  pollutant = "CO",
  by = "streets"
)

x_st <- st_sf(gCO = Emissions(rowSums(x_STREETS, na.rm = T), 
                              mass = "g", 
                              time = "h"),
              geometry = net$geometry)

g <- make_grid(spobj = x_st, width = 1/102.42/2)

gco <- emis_grid(spobj = x_st, g = g)

# plot

plot(net["ldv"], 
     axes = F, 
     pal = cpt("mpl_viridis",colorRampPalette = T, rev = T),
     lwd = 2, 
     key.pos = 1)

plot(x_st["gCO"], 
     axes = T, 
     pal = cpt("mpl_viridis",colorRampPalette = T, rev = T))

plot(gco["gCO"], 
     axes = T, 
     pal = cpt("mpl_viridis",colorRampPalette = T, rev = T))



png("chapter3_fig_veh_1.png", width = 1300, height = 1000, res = 300)
plot(net["ldv"], 
     pal = cpt("mpl_viridis",
               colorRampPalette = T, 
               rev = T),
     lwd = 2, 
     key.pos = 1)

dev.off()



png("chapter3_fig_veh_2.png", width = 1300, height = 1000, res = 300)
plot(x_st["gCO"], 
     pal = cpt("mpl_viridis",
               colorRampPalette = T, 
               rev = T),
     lwd = 2, 
     key.pos = 1)

dev.off()



png("chapter3_fig_veh_3.png", width = 1300, height = 1000, res = 300)
plot(gco["gCO"], 
     pal = cpt("mpl_viridis",
               colorRampPalette = T, 
               rev = T),
     lwd = 2,
     lty = 0,
     key.pos = 1,)

dev.off()


fn <- function(x, label, size = 50) {
  f <-magick::image_read(x)
  f <- magick::image_trim(f)
  f <- magick::image_annotate(f, 
                         text = label, 
                         size = size)
  
  magick::image_write(image = f, path = x)
}

fn(x = "chapter3_fig_veh_1.png", label = "a)")
fn(x = "chapter3_fig_veh_2.png", label = "b)")
fn(x = "chapter3_fig_veh_3.png", label = "c)")

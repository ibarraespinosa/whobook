library(vein)


# Emission factor function
ef1 <- ef_ldv_speed(v = "PC",
                    t = "4S", 
                    cc = "<=1400", 
                    f = "G", 
                    eu = "IV",
                    p = "CO2",
                    speed = Speed(0:150))

ef2 <- ef_ldv_speed(v = "PC",
                    t = "4S", 
                    cc = "1400_2000", 
                    f = "G", 
                    eu = "IV",
                    p = "CO2",
                    speed = Speed(0:150))

ef3 <- ef_ldv_speed(v = "PC",
                    t = "4S", 
                    cc = ">2000", 
                    f = "G", 
                    eu = "IV",
                    p = "CO2",
                    speed = Speed(0:150))


df <- data.frame(small = ef1,
                 medium = ef2,
                 big = ef3)

colplot(df, 
        ylab = expression(CO[2]~g/km), 
        xlab = "km/h", 
        theme = "clean")

png("chapter3_co2_road.png", width = 2000, height = 1500, res = 300)
colplot(df, 
        ylab = expression(CO[2]~g/km), 
        xlab = "km/h", 
        theme = "clean")
dev.off()


fn <- function(x, label, size = 50) {
  f <-magick::image_read(x)
  f <- magick::image_trim(f)
  f <- magick::image_annotate(f, 
                              text = label, 
                              size = size)
  
  magick::image_write(image = f, path = x)
}

fn(x = "chapter3_co2_road.png", label = "")


sapply(df, mean)

df$speed <- 0:150

df[df$speed == 33, ] |> unlist() |> round(2)

df[df$small == min(df$small), ] |> unlist() |> round(2)
df[df$medium == min(df$medium), ] |> unlist() |> round(2)
df[df$big == min(df$big), ] |> unlist() |> round(2)

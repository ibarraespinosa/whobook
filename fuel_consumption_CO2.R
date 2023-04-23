library(vein)


# Emission factor function
ef1 <- ef_ldv_speed(v = "PC",
                    t = "4S", 
                    cc = "<=1400", 
                    f = "G", 
                    eu = "IV",
                    p = "FC",
                    speed = Speed(0:150))

ef2 <- ef_ldv_speed(v = "PC",
                    t = "4S", 
                    cc = "1400_2000", 
                    f = "G", 
                    eu = "IV",
                    p = "FC",
                    speed = Speed(0:150))

ef3 <- ef_ldv_speed(v = "PC",
                    t = "4S", 
                    cc = ">2000", 
                    f = "G", 
                    eu = "IV",
                    p = "FC",
                    speed = Speed(0:150))


df <- data.frame(small = ef1,
                 medium = ef2,
                 big = ef3)
colplot(df, 
        ylab = "Fuel Consumption g/km", 
        xlab = "km/h", 
        theme = "clean")

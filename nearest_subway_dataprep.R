setwd("/Users/ewelinka/Desktop/Housing-Prices-Spatial-econometrics")

df_final <- readxl::read_excel("df_final_v2.xlsx")
summary(df_final)

metro <- data.frame(station = c("Kabaty", "Natolin", "Imielin", "Stokłosy", "Ursynów", "Służew", "Wilanowska", 
                                "Wierzbno", "Racławicka", "Pole_Mokotowskie", "Politechnika", "Centrum", "Świętokrzyska",
                                "Ratusz_Arsenał", "Dworzec_Gdański", "Plac_Wilsona", "Marymont", "Słodowiec", "Stare_Bielany", 
                                "Wawrzyszew", "Młociny", 
                                "Księcia_Janusza", "Młynów", "Płocka", "Rondo_Daszyńskiego", "Rondo_ONZ", 
                                "Nowy_Świat", "CNK", "Stadion_Narodowy", "Dworzec_Wileński", "Szwedzka", "Targówek", "Trocka"),
                    lat = c(52.1314124, 52.1396902, 52.1500899, 52.1561292, 52.1613618, 52.1733019, 52.1811072,
                            52.1904197,52.1991004,52.2088425,52.2166845,52.216679,52.2352732,52.2448043,52.2575656,
                            52.2688336,52.2717241,52.2759965,52.2801845,52.28323,52.2868791,
                            52.239035,52.2377768,52.2327241,52.2299872,52.2329508,52.2364112,52.2394039,52.2472444,
                            52.2540787,52.2570703,52.2694184,52.2738826), 
                    lon = c(21.065519, 21.0561986, 21.0432985, 21.0338946, 21.0270013, 21.0264675, 21.0217164,
                            21.0145773,21.0122759,21.0054488,21.0165821,20.9902102,21.0063005,21.0017348,20.9935019,
                            20.9829901,20.9711893,20.9589334,20.9508653,20.9408017, 20.9341498,
                            20.9425829,20.9587245,20.9643946,20.982226,20.9976487,21.0146957,21.0301882,21.0424888,
                            21.0352468,21.0251564,21.0507258,21.0510128))

dist = list()
for (i in 1:as.numeric(dim(metro)[1])){
  test <- distHaversine(df_final[,15:16], metro[i,2:3])/1000
  dist[[metro[i,1]]] <- test
}

dist_all <- do.call(cbind, dist)
dist_all <- as.data.frame(dist_all)
dist_all$Distance_to_subway_in_km <- apply(dist_all, 1, FUN=min)

df_final <- cbind(df_final, dist_all$Distance_to_subway_in_km)
colnames(df_final)[26] <- "Distance_to_subway_in_km"

install.packages("xlsx")
library(xlsx)
write.xlsx(df_final, file = "df_final_v3.xlsx")






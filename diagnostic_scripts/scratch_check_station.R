library(readxl)
df <- read_excel("/home/marc/Documents/Agroscope/Meteo_Reckenholz/Meteodaten_C_2020_Monate.xlsx")
print(unique(df$Kurz))
print(unique(df$Station))

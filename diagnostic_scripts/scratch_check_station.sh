cat << 'EOF' > scratch_check_station.R
library(readxl)
df <- read_excel("/home/marc/Documents/Agroscope/Meteo_Reckenholz/Meteodaten_C_2020_Monate.xlsx")
print(unique(df$Kurz))
print(unique(df$Station))
EOF
Rscript scratch_check_station.R

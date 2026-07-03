cat << 'EOF' > scratch_check_older.R
library(readxl)
df <- read_excel("/home/marc/Documents/Agroscope/Meteo_Reckenholz/Meteodaten_2004_Monate.xls")
print(colnames(df))
print(head(df))

df2 <- read_excel("/home/marc/Documents/Agroscope/Meteo_Reckenholz/Meteodaten_B_2012_Monate.xls")
print(colnames(df2))
print(head(df2))
EOF
Rscript scratch_check_older.R

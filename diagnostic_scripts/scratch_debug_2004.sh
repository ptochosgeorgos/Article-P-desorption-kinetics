cat << 'EOF' > scratch_debug_2004.R
library(readxl)
library(dplyr)
f <- "/home/marc/Documents/Agroscope/Meteo_Reckenholz/Meteodaten_2004_Monate.xls"
df <- read_excel(f)
cat("Colnames:\n")
print(colnames(df))
if ("Station" %in% colnames(df)) {
    cat("\nStation unique values:\n")
    print(unique(df$Station))
    df_filtered <- df |> filter(Station == "Zürich / Affoltern")
    cat("\nFiltered rows:\n")
    print(nrow(df_filtered))
}

EOF
Rscript scratch_debug_2004.R

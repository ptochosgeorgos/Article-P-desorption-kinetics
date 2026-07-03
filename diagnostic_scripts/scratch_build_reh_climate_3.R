library(readxl)
library(dplyr)
library(purrr)

files <- list.files("/home/marc/Documents/Agroscope/Meteo_Reckenholz", pattern = "_Monate\\.xls", full.names = TRUE)

reh_monthly <- map_dfr(files, function(f) {
    tryCatch({
        df <- read_excel(f)
        
        # Check if we can find Reckenholz
        df_sub <- NULL
        if ("Kurz" %in% colnames(df)) {
            df_sub <- df |> filter(Kurz == "REH" | grepl("Reckenholz|Affoltern", Station))
        } else if ("Station" %in% colnames(df)) {
            df_sub <- df |> filter(grepl("Reckenholz|Affoltern", Station))
        }
        
        if (is.null(df_sub) || nrow(df_sub) == 0) return(NULL)
        
        # We need the year from the filename
        year <- as.numeric(gsub(".*_([0-9]{4})_Monate.*", "\\1", f))
        
        # Select the relevant columns
        df_sub <- df_sub |>
            select(contains("Tmittel 2m"), contains("Niederschlag mm"))
            
        # Standardize names
        colnames(df_sub) <- c("Temp", "Prec")
        df_sub$year <- year
        return(df_sub)
        
    }, error = function(e) {
        cat("Error reading", f, "\n")
        return(NULL)
    })
})

# Calculate annual means and sums
reh_annual <- reh_monthly |>
    group_by(year) |>
    summarise(
        anavg_temp = mean(Temp, na.rm = TRUE),
        ansum_prec = sum(Prec, na.rm = TRUE)
    ) |>
    mutate(site = "REH")

reh_annual <- reh_annual |>
    mutate(
        juvdev_temp = anavg_temp,
        juvdev_prec = ansum_prec
    )

print("REH Annual Climate Data Generated (All Years):")
print(reh_annual, n=100)

all_p <- readRDS("data/all_P.rds")
all_p_clean <- all_p |> filter(site != "REH")

new_rows <- data.frame(
    site = "REH",
    year = reh_annual$year,
    anavg_temp = reh_annual$anavg_temp,
    ansum_prec = reh_annual$ansum_prec,
    juvdev_temp = reh_annual$juvdev_temp,
    juvdev_prec = reh_annual$juvdev_prec
)

missing_cols <- setdiff(colnames(all_p_clean), colnames(new_rows))
for(col in missing_cols) {
    new_rows[[col]] <- NA
}

all_p_combined <- bind_rows(all_p_clean, new_rows)
saveRDS(all_p_combined, "data/all_P.rds")
cat("Successfully updated data/all_P.rds with complete REH climate data!\n")


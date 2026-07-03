library(readxl)
library(dplyr)
library(purrr)

# Get all monthly meteo files
files <- list.files("/home/marc/Documents/Agroscope/Meteo_Reckenholz", pattern = "_Monate\\.xls", full.names = TRUE)

reh_monthly <- map_dfr(files, function(f) {
    tryCatch({
        df <- read_excel(f)
        # Handle cases where column names might differ slightly
        if ("Kurz" %in% colnames(df)) {
            df <- df |> filter(Kurz == "REH")
        } else {
            return(NULL)
        }
        
        # We need the year from the filename
        year <- as.numeric(gsub(".*_([0-9]{4})_Monate.*", "\\1", f))
        
        # Select the relevant columns
        # Tmittel 2m
        # Niederschlag mm
        df |>
            select(contains("Tmittel 2m"), contains("Niederschlag")) |>
            mutate(year = year)
    }, error = function(e) {
        cat("Error reading", f, "\n")
        return(NULL)
    })
})

# Some files might have different names for temp and prec
# Let's clean up column names
colnames(reh_monthly) <- c("Temp", "Prec", "year")

# Calculate annual means and sums
reh_annual <- reh_monthly |>
    group_by(year) |>
    summarise(
        anavg_temp = mean(Temp, na.rm = TRUE),
        ansum_prec = sum(Prec, na.rm = TRUE)
    ) |>
    mutate(site = "REH")

# Calculate anomalies
long_term_temp <- mean(reh_annual$anavg_temp, na.rm = TRUE)
long_term_prec <- mean(reh_annual$ansum_prec, na.rm = TRUE)

reh_annual <- reh_annual |>
    mutate(
        juvdev_temp = anavg_temp, # The anomaly is calculated later in D_ready by juvdev_temp - mean(juvdev_temp)
        juvdev_prec = ansum_prec  # So we just provide the raw values here as juvdev
    )

print("REH Annual Climate Data Generated:")
print(reh_annual)

# Load existing climate data
all_p <- readRDS("data/all_P.rds")

# Remove any existing REH data just in case
all_p_clean <- all_p |> filter(site != "REH")

# Because all_p might have many other columns, we just select the ones we need for the bind
# Actually, the base script does:
# climate_data <- readRDS("data/all_P.rds") |> dplyr::select(site, year, anavg_temp, ansum_prec, juvdev_temp, juvdev_prec)

# We can just append the new REH data to all_P.rds
# But let's make sure we have matching columns.
new_rows <- data.frame(
    site = "REH",
    year = reh_annual$year,
    anavg_temp = reh_annual$anavg_temp,
    ansum_prec = reh_annual$ansum_prec,
    juvdev_temp = reh_annual$juvdev_temp,
    juvdev_prec = reh_annual$juvdev_prec
)

# Fill other columns with NA
missing_cols <- setdiff(colnames(all_p_clean), colnames(new_rows))
for(col in missing_cols) {
    new_rows[[col]] <- NA
}

all_p_combined <- bind_rows(all_p_clean, new_rows)

saveRDS(all_p_combined, "data/all_P.rds")
cat("Successfully updated data/all_P.rds with REH climate data!\n")


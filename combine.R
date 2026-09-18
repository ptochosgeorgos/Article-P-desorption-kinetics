lines_bio <- readLines("k_biological_modelling.qmd")
lines_gapon <- readLines("code_gapon.qmd")

idx_lme4 <- grep("library\\(lme4\\)", lines_gapon)[1]
if (!any(grepl("library\\(nlme\\)", lines_gapon))) {
  lines_gapon <- c(lines_gapon[1:idx_lme4], "library(nlme)", lines_gapon[(idx_lme4+1):length(lines_gapon)])
}

start_idx <- grep("## 1 - Deriving the Buffer Capacity", lines_bio)[1]
bio_part <- lines_bio[start_idx:length(lines_bio)]

lines_gapon <- c(lines_gapon, "", "---", "", "# PART II: Biological Modeling & Agronomic Application", "", bio_part)

writeLines(lines_gapon, "code_gapon.qmd")

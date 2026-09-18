library(nlme)
set.seed(123)
n <- 100
yield_data <- data.frame(
  annual_yield_mp_DM = runif(n, 5, 15),
  z_AR_K = runif(n, -2, 2),
  z_b = runif(n, -2, 2),
  crop = factor(rep(c("Wheat", "Maize"), each = 50)),
  site = factor(rep(c("A", "B"), each = 50)),
  year = factor(rep(2010:2019, 10)) # Creates empty groups!
)

# Convert crop to dummy variables
yield_data$crop_Wheat <- ifelse(yield_data$crop == "Wheat", 1, 0)
yield_data$crop_Maize <- ifelse(yield_data$crop == "Maize", 1, 0)

k_model <- function(z_AR_K, z_b, A, Y_0, c_I, c_b) { Y_0 + (A - Y_0) * (1 - exp(-(c_I * z_AR_K + c_b * z_b))) }

start_A <- rep(10, 2)
start_Y0 <- rep(5, 2)
start_c_I <- rep(0.1, 2)
start_c_b <- rep(0.1, 2)
my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)

tryCatch({
  m <- nlme(
    annual_yield_mp_DM ~ k_model(z_AR_K, z_b, A, Y_0, c_I, c_b),
    data = yield_data,
    fixed = list(A ~ crop_Wheat + crop_Maize - 1, 
                 Y_0 ~ crop_Wheat + crop_Maize - 1, 
                 c_I ~ crop_Wheat + crop_Maize - 1, 
                 c_b ~ crop_Wheat + crop_Maize - 1),
    random = Y_0 ~ 1 | site/year,
    start = my_starts,
    control = nlmeControl(pnlsMaxIter = 1)
  )
  print("Success")
}, error = function(e) print(e))

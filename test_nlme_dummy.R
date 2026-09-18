library(nlme)
set.seed(123)
n <- 100
yield_data <- data.frame(
  annual_yield_mp_DM = runif(n, 5, 15),
  z_AR_K = runif(n, -2, 2),
  z_b = runif(n, -2, 2),
  crop = factor(rep(c("Wheat", "Maize", "Potato", "Soybean", "Rice"), each = 20)),
  site = factor(rep(c("A", "B", "C", "D"), each = 25)),
  year = factor(rep(2010:2019, 10))
)

k_model <- function(z_AR_K, z_b, A, Y_0, c_I, c_b) {
  Y_0 + (A - Y_0) * (1 - exp(-(c_I * z_AR_K + c_b * z_b)))
}

num_crops <- 5
start_A <- c(10, rep(0, num_crops - 1))
start_Y0 <- c(5, rep(0, num_crops - 1))
start_c_I <- c(0.1, rep(0, num_crops - 1))
start_c_b <- c(0.1, rep(0, num_crops - 1))
my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)

tryCatch({
  m_nlme_yield <- nlme(
    annual_yield_mp_DM ~ k_model(z_AR_K, z_b, A, Y_0, c_I, c_b),
    data = yield_data,
    fixed = A + Y_0 + c_I + c_b ~ crop,
    random = Y_0 ~ 1 | site/year,
    start = my_starts
  )
  print("Success")
}, error = function(e) {
  print(e)
})

library(nlme)
set.seed(123)
n <- 100
yield_data <- data.frame(
  annual_yield_mp_DM = runif(n, 5, 15),
  z_AR_K = runif(n, -2, 2),
  z_b = runif(n, -2, 2),
  crop = factor(rep(letters[1:13], length.out = n)),
  site = factor(rep(c("A", "B"), each = 50)),
  year = factor(rep(2010:2019, 10))
)

# Convert crop to dummy variables with LONG names
dummy_mat <- model.matrix(~ 0 + crop, yield_data)
colnames(dummy_mat) <- paste0("cropSuperLongNameThatWillTakeSpace_", 1:13)
yield_data <- cbind(yield_data, as.data.frame(dummy_mat))

k_model <- function(z_AR_K, z_b, A, Y_0, c_I, c_b) { Y_0 + (A - Y_0) * (1 - exp(-(c_I * z_AR_K + c_b * z_b))) }

start_A <- rep(10, 13)
start_Y0 <- rep(5, 13)
start_c_I <- rep(0.1, 13)
start_c_b <- rep(0.1, 13)
my_starts <- c(start_A, start_Y0, start_c_I, start_c_b)

fixed_f <- paste(colnames(dummy_mat), collapse = " + ")
my_fixed <- list(
  as.formula(paste("A ~", fixed_f, "- 1")),
  as.formula(paste("Y_0 ~", fixed_f, "- 1")),
  as.formula(paste("c_I ~", fixed_f, "- 1")),
  as.formula(paste("c_b ~", fixed_f, "- 1"))
)

tryCatch({
  m <- nlme(
    annual_yield_mp_DM ~ k_model(z_AR_K, z_b, A, Y_0, c_I, c_b),
    data = yield_data,
    fixed = my_fixed,
    random = Y_0 ~ 1 | site/year,
    start = my_starts,
    control = nlmeControl(pnlsMaxIter = 1)
  )
  print("Success")
}, error = function(e) print(e))

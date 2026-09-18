source("scratch/qi_code_subset.R")
cat("C_cons parameters:\n")
print(fixef(m_PTF_cons_co2))
cat("C_cons(\"z_ln_Corg\"):", C_cons("z_ln_Corg"), "\n")
cat("C_cons(\"z_ln_FineTexture\"):", C_cons("z_ln_FineTexture"), "\n")
cat("get_int_cons(\"ln_P_CO2\", \"z_ln_FineTexture\"):", get_int_cons("ln_P_CO2", "z_ln_FineTexture"), "\n")

# Try to find exactly which term is NA!
D_test <- D_ready |> head(5)
D_test$test_corg <- C_cons("z_ln_Corg") * D_test$z_ln_Corg
D_test$test_k <- C_cons("z_ln_K") * D_test$z_ln_K
print(D_test$test_corg)
print(D_test$test_k)


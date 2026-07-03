packages <- c("MuMIn", "performance", "Metrics")
for (p in packages) {
    cat(p, ": ", requireNamespace(p, quietly = TRUE), "\n")
}

cat << 'EOF' > scratch_check_packages.R
packages <- c("MuMIn", "performance", "Metrics")
for (p in packages) {
    cat(p, ": ", requireNamespace(p, quietly = TRUE), "\n")
}
EOF
Rscript scratch_check_packages.R

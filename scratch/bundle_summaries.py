lines = open("scratch/train_ptf.R").read().split("\n")
output = []
for line in lines:
    if "## ---- plant-uptake-showdown" in line:
        break
    output.append(line)

output.append("""
# Load existing models
final_artifacts <- readRDS("data/Final_Models_Data.rds")
m_yield <- final_artifacts$models$yield_raw_co2
m_uptake <- final_artifacts$models$uptake_raw_co2

# Save the 4 models
saveRDS(list(
    k_ptf = k_ptf,
    ptf_cons_thm = ptf_cons_thm,
    yield = m_yield,
    uptake = m_uptake
), "data/Summary_Models.rds")
print("Saved Summary_Models.rds successfully.")
""")

with open("scratch/bundle_summaries.R", "w") as f:
    f.write("\n".join(output))

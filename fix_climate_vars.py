import re

files = ["notebooks/cluster_bayesian_offload.R", "notebooks/cluster_bayesian_offload.qmd"]

for file in files:
    with open(file, "r") as f:
        content = f.read()

    # Replace in bform_Y_mech
    content = content.replace("betaTemp * z_Temp_Mean + betaPrec * z_Prec_Anom", "betaTemp * juvdev_temp + betaPrec * juvdev_prec")
    
    # Replace in bform_U_mech
    content = content.replace("betaTemp * rollMean_Temp + betaPrec * rollMean_Prec", "betaTemp * juvdev_temp + betaPrec * juvdev_prec")
    
    with open(file, "w") as f:
        f.write(content)

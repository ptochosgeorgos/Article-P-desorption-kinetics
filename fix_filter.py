import re

files = ["notebooks/cluster_bayesian_offload.R", "notebooks/cluster_bayesian_offload.qmd"]

for file in files:
    with open(file, "r") as f:
        content = f.read()

    # The block currently looks like:
    # d_brms <- filter(
    #   mutate(...),
    #   is.finite(C_P), C_P > 0, 
    #   !is.na(annual_yield_mp_DM), 
    #   !is.na(annual_P_uptake), 
    #   !is.na(z_inv_b),
    #   !is.na(Y_ref),
    #   !is.na(soil_0_20_P_CO2),
    #   !is.na(juvdev_temp),
    #   !is.na(juvdev_prec)
    # )

    old_filter = """
    filter(
      is.finite(C_P), C_P > 0, 
      !is.na(annual_yield_mp_DM), 
      !is.na(annual_P_uptake), 
      !is.na(z_inv_b),
      !is.na(Y_ref),
      !is.na(soil_0_20_P_CO2),
      !is.na(juvdev_temp),
      !is.na(juvdev_prec)
    )
"""
    new_filter = """
    filter(
      !is.na(annual_yield_mp_DM), 
      !is.na(z_inv_b),
      !is.na(Y_ref),
      !is.na(soil_0_20_P_CO2),
      !is.na(juvdev_temp),
      !is.na(juvdev_prec)
    )
"""
    # Wait, the exact spacing might differ. Let's use regex.
    pattern = re.compile(r'filter\([^)]+is\.finite\(C_P\)[^)]+\)', re.DOTALL)
    
    # Actually just string replacement:
    content = re.sub(
        r'is\.finite\(C_P\),\s*C_P > 0,\s*!is\.na\(annual_yield_mp_DM\),\s*!is\.na\(annual_P_uptake\)',
        r'!is.na(annual_yield_mp_DM)',
        content
    )

    with open(file, "w") as f:
        f.write(content)

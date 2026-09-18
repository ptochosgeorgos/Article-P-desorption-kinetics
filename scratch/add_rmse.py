import re
with open("notebooks/cluster_bayesian_offload.R", "r") as f:
    script = f.read()

# Add get_rmse function at the top
rmse_func = """
get_rmse <- function(mod, y) {
  preds <- fitted(mod)[, "Estimate"]
  sqrt(mean((y - preds)^2, na.rm = TRUE))
}
"""
if "get_rmse <- function" not in script:
    script = re.sub(r'(# HMC config\ncores_n = 16\nchains_n = 4\nthreads_n = 4\niter_n = 2000)', rmse_func + r'\1', script)

# Inject rmse calculations before rm()
script = script.replace('rm(mod_base_Y); gc()', 'rmse_base <- get_rmse(mod_base_Y, d_brms$annual_yield_mp_DM)\nrm(mod_base_Y); gc()')
script = script.replace('rm(mod_base_U); gc()', 'rmse_base_U <- get_rmse(mod_base_U, d_brms$annual_P_uptake)\nrm(mod_base_U); gc()')

script = script.replace('rm(mod_null_Y); gc()', 'rmse_null <- get_rmse(mod_null_Y, d_brms$annual_yield_mp_DM)\nrm(mod_null_Y); gc()')
script = script.replace('rm(mod_null_U); gc()', 'rmse_null_U <- get_rmse(mod_null_U, d_brms$annual_P_uptake)\nrm(mod_null_U); gc()')

script = script.replace('rm(mod_heur_Y); gc()', 'rmse_heur <- get_rmse(mod_heur_Y, d_brms$annual_yield_mp_DM)\nrm(mod_heur_Y); gc()')
script = script.replace('rm(mod_heur_U); gc()', 'rmse_heur_U <- get_rmse(mod_heur_U, d_brms$annual_P_uptake)\nrm(mod_heur_U); gc()')

script = script.replace('rm(mod_mech_Y); gc()', 'rmse_mech <- get_rmse(mod_mech_Y, d_brms$annual_yield_mp_DM)\nrm(mod_mech_Y); gc()')
script = script.replace('rm(mod_mech_U); gc()', 'rmse_mech_U <- get_rmse(mod_mech_U, d_brms$annual_P_uptake)\nrm(mod_mech_U); gc()')

# Add rmse to export payload
payload_old = """export_payload <- list(
    yield = list(
        comparison = comp_yield,
        plot_data = list(null = ce_null[[1]], heur = ce_heur[[1]], mech = ce_mech[[1]]),
        parameters = list(mech = params_mech, heur = params_heur),
        r2_mech = r2_mech
    ),
    uptake = list(
        comparison = comp_uptake,
        plot_data = list(null = ce_null_U[[1]], heur = ce_heur_U[[1]], mech = ce_mech_U[[1]]),
        parameters = list(mech = params_mech_U, heur = params_heur_U),
        r2_mech = r2_mech_U
    )
)"""

payload_new = """export_payload <- list(
    yield = list(
        comparison = comp_yield,
        plot_data = list(null = ce_null[[1]], heur = ce_heur[[1]], mech = ce_mech[[1]]),
        parameters = list(mech = params_mech, heur = params_heur),
        r2_mech = r2_mech,
        rmse = list(base = rmse_base, null = rmse_null, heur = rmse_heur, mech = rmse_mech)
    ),
    uptake = list(
        comparison = comp_uptake,
        plot_data = list(null = ce_null_U[[1]], heur = ce_heur_U[[1]], mech = ce_mech_U[[1]]),
        parameters = list(mech = params_mech_U, heur = params_heur_U),
        r2_mech = r2_mech_U,
        rmse = list(base = rmse_base_U, null = rmse_null_U, heur = rmse_heur_U, mech = rmse_mech_U)
    )
)"""

script = script.replace(payload_old, payload_new)

with open("notebooks/cluster_bayesian_offload.R", "w") as f:
    f.write(script)
print("Updated cluster_bayesian_offload.R")

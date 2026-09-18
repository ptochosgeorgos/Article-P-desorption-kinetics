import re

with open("notebooks/cluster_bayesian_offload.R", "r") as f:
    script = f.read()

# Change fixef to summary()$fixed for diagnostics
script = script.replace('params_heur <- fixef(mod_heur_Y)', 'params_heur <- summary(mod_heur_Y)$fixed\n  r2_heur <- list(conditional = bayes_R2(mod_heur_Y), marginal = bayes_R2(mod_heur_Y, re_formula = NA))')
script = script.replace('params_heur_U <- fixef(mod_heur_U)', 'params_heur_U <- summary(mod_heur_U)$fixed\n  r2_heur_U <- list(conditional = bayes_R2(mod_heur_U), marginal = bayes_R2(mod_heur_U, re_formula = NA))')
script = script.replace('params_mech <- fixef(mod_mech_Y)', 'params_mech <- summary(mod_mech_Y)$fixed')
script = script.replace('params_mech_U <- fixef(mod_mech_U)', 'params_mech_U <- summary(mod_mech_U)$fixed')

# Change r2_mech to list of conditional and marginal
script = script.replace('r2_mech <- bayes_R2(mod_mech_Y)', 'r2_mech <- list(conditional = bayes_R2(mod_mech_Y), marginal = bayes_R2(mod_mech_Y, re_formula = NA))')
script = script.replace('r2_mech_U <- bayes_R2(mod_mech_U)', 'r2_mech_U <- list(conditional = bayes_R2(mod_mech_U), marginal = bayes_R2(mod_mech_U, re_formula = NA))')

# Update payload to include r2_heur
script = script.replace('r2_mech = r2_mech,', 'r2_mech = r2_mech,\n        r2_heur = r2_heur,')
script = script.replace('r2_mech = r2_mech_U,', 'r2_mech = r2_mech_U,\n        r2_heur = r2_heur_U,')

with open("notebooks/cluster_bayesian_offload.R", "w") as f:
    f.write(script)
print("Updated metrics in cluster_bayesian_offload.R")

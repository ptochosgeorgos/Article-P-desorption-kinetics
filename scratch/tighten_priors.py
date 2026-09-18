import re

with open("notebooks/cluster_bayesian_offload.R", "r") as f:
    script = f.read()

# Fix Yield Priors
script = script.replace('prior(normal(50, 50), nlpar = "Y0", lb = 0)', 'prior(normal(100, 400), nlpar = "Y0", lb = 0)')
script = script.replace('prior(normal(100, 50), nlpar = "A", lb = 0)', 'prior(normal(200, 400), nlpar = "A", lb = 0)')

# Fix Uptake Priors
script = script.replace('prior(normal(30, 15), nlpar = "Vmax", lb = 0)', 'prior(normal(30, 40), nlpar = "Vmax", lb = 0)')

# Tighten the exponential modifiers
script = script.replace('prior(normal(0, 1), nlpar = "betainvb")', 'prior(normal(0, 0.5), nlpar = "betainvb")')
script = script.replace('prior(normal(0, 1), nlpar = "betak")', 'prior(normal(0, 0.5), nlpar = "betak")')
script = script.replace('prior(normal(0, 1), nlpar = "betaN")', 'prior(normal(0, 0.5), nlpar = "betaN")')
script = script.replace('prior(normal(0, 1), nlpar = "betaTemp")', 'prior(normal(0, 0.5), nlpar = "betaTemp")')
script = script.replace('prior(normal(0, 1), nlpar = "betaPrec")', 'prior(normal(0, 0.5), nlpar = "betaPrec")')
script = script.replace('prior(normal(0, 1), nlpar = "betapH")', 'prior(normal(0, 0.5), nlpar = "betapH")')
script = script.replace('prior(normal(0, 1), nlpar = "betaClay")', 'prior(normal(0, 0.5), nlpar = "betaClay")')

with open("notebooks/cluster_bayesian_offload.R", "w") as f:
    f.write(script)
print("Priors tightened.")

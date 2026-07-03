import pandas as pd
tables = pd.read_html('docs/notebooks/qi_modelling1.html')
for i, t in enumerate(tables):
    if 'Model' in t.columns and 'Marginal_R2' in t.columns:
        print(f"\n--- TABLE {i} ---")
        print(t.to_string())

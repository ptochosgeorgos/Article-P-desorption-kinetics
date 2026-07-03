import pandas as pd
tables = pd.read_html('docs/notebooks/qi_modelling1.html')
for i, t in enumerate(tables):
    if t.astype(str).apply(lambda col: col.str.contains('Freundlich n - Raw P_CO2').any()).any():
        print(f"\n--- TABLE {i} (Contains Freundlich) ---")
        print(t.to_string())

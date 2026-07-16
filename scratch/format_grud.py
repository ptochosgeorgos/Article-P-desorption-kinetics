matrix_text = """
0-4.9: 1.5, 1.5, 1.5, 1.4, 1.4
5.0-9.9: 1.5, 1.5, 1.4, 1.4, 1.2
10.0-14.9: 1.5, 1.4, 1.4, 1.2, 1.2
15.0-19.9: 1.4, 1.4, 1.2, 1.2, 1.0
20.0-24.9: 1.4, 1.2, 1.2, 1.0, 1.0
25.0-29.9: 1.2, 1.2, 1.2, 1.0, 1.0
30.0-34.9: 1.2, 1.0, 1.0, 1.0, 1.0
35.0-39.9: 1.2, 1.0, 1.0, 1.0, 1.0
40.0-44.9: 1.0, 1.0, 1.0, 1.0, 1.0
45.0-49.9: 1.0, 1.0, 1.0, 1.0, 1.0
50.0-54.9: 1.0, 1.0, 1.0, 0.8, 0.8
55.0-59.9: 1.0, 1.0, 0.8, 0.8, 0.6
60.0-64.9: 1.0, 1.0, 0.8, 0.8, 0.6
65.0-69.9: 1.0, 0.8, 0.8, 0.6, 0.6
70.0-74.9: 0.8, 0.8, 0.8, 0.6, 0.6
75.0-79.9: 0.8, 0.8, 0.6, 0.6, 0.4
80.0-84.9: 0.8, 0.6, 0.6, 0.4, 0.4
85.0-89.9: 0.6, 0.6, 0.6, 0.4, 0.4
90.0-94.9: 0.6, 0.6, 0.4, 0.4, 0.0
95.0-99.9: 0.6, 0.4, 0.4, 0.0, 0.0
100.0-104.9: 0.4, 0.4, 0.4, 0.0, 0.0
105.0-109.9: 0.4, 0.4, 0.0, 0.0, 0.0
110.0-114.9: 0.4, 0.0, 0.0, 0.0, 0.0
115.0-119.9: 0.0, 0.0, 0.0, 0.0, 0.0
120.0-124.9: 0.0, 0.0, 0.0, 0.0, 0.0
≥ 125.0: 0.0, 0.0, 0.0, 0.0, 0.0
"""

lines = matrix_text.strip().split('\n')
labels = []
matrix_js = "grud_aae_matrix = [\n"
matrix_r = "z_aae10 <- matrix(c(\n"

thresholds = []

for i, line in enumerate(lines):
    label, vals = line.split(': ')
    labels.append(f'"{label}"')
    
    # parse thresholds
    if '-' in label:
        t = float(label.split('-')[1])
        thresholds.append(t)
    
    matrix_js += f"  [{vals}],\n"
    matrix_r += f"  {vals},\n"

matrix_js = matrix_js.rstrip(',\n') + "\n];"
matrix_r = matrix_r.rstrip(',\n') + "\n), nrow=26, ncol=5, byrow=TRUE)"

print("R Labels:")
print(f"y_aae10 <- c({', '.join(labels)})")
print()
print("R Matrix:")
print(matrix_r)
print()
print("JS Matrix:")
print(matrix_js)
print()
print("JS Thresholds:")
print(f"let thresholds = [{', '.join(map(str, thresholds))}];")


import re

crops = ["Winterweizen", "Sommerweizen", "Körnermais", "Silomais", "Kartoffeln", "Zuckerrüben", "Raps", "Soja", "Wintergerste", "Dinkel", "Kunstwiese", "Futterrüben", "Chicor", "Zwischen", "Erbsen"]

with open("scratch/grud.txt", "r") as f:
    lines = f.readlines()

in_table = False
for line in lines:
    if "Tabelle 9 | Referenzertrag" in line:
        in_table = True
    if "Tabelle 10 |" in line:
        in_table = False
        
    if in_table:
        for c in crops:
            if c in line:
                print(line.strip())

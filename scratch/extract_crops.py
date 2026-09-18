import subprocess
import re

pdf_path = "/home/marc/.gemini/antigravity-ide/brain/c9f9c6f8-e0ab-4a3a-92f3-7f5c404064e6/media__1784142891314.pdf"
res = subprocess.run(["pdftotext", "-layout", pdf_path, "-"], capture_output=True, text=True)

# Find page with "Tabelle 9 | Referenzertrag" and extract
capture = False
table_text = []

for line in res.stdout.split('\n'):
    if "Tabelle 9 | Referenzertrag, Nährstoffentzug und Düngungsnormen" in line:
        capture = True
    if capture:
        table_text.append(line)
        if "Tabelle 10" in line:
            break

print("\n".join(table_text[:100]))

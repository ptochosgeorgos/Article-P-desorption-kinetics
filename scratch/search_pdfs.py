import glob
import subprocess

pdf_files = glob.glob("/home/marc/.gemini/antigravity-ide/brain/c9f9c6f8-e0ab-4a3a-92f3-7f5c404064e6/*.pdf")
for pdf in pdf_files:
    try:
        res = subprocess.run(["pdftotext", pdf, "-"], capture_output=True, text=True)
        lines = res.stdout.split('\n')
        for i, line in enumerate(lines):
            if "Tabelle 9" in line or "Tab. 9" in line:
                print(f"Found in {pdf} at line {i}: {line}")
                print("\n".join(lines[max(0, i-5):min(len(lines), i+30)]))
                print("-" * 50)
    except Exception as e:
        print(f"Error on {pdf}: {e}")

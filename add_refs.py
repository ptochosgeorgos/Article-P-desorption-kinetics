import urllib.request
import urllib.parse

def get_bibtex(query):
    url = f"https://api.crossref.org/works?query={urllib.parse.quote(query)}&select=DOI&rows=1"
    try:
        import json
        req = urllib.request.Request(url, headers={'User-Agent': 'mailto:research@example.com'})
        with urllib.request.urlopen(req) as response:
            data = json.loads(response.read().decode())
            if data['message']['items']:
                doi = data['message']['items'][0]['DOI']
                bib_req = urllib.request.Request(f"https://api.crossref.org/works/{doi}/transform/application/x-bibtex")
                with urllib.request.urlopen(bib_req) as bib_response:
                    return bib_response.read().decode('utf-8')
    except Exception as e:
        print(f"Error for {query}: {e}")
    return ""

refs = [
    "Barber Soil nutrient bioavailability: a mechanistic approach 1995",
    "Michaelis Menten Die Kinetik der Invertinwirkung 1913",
    "Freundlich adsorption in solution 1906"
]

with open("references.bib", "a", encoding="utf-8") as f:
    for r in refs:
        bib = get_bibtex(r)
        print(bib)
        if bib:
            f.write("\n" + bib + "\n")
            
# Since Crossref might not have the 1906/1913 papers perfectly or the book, we will manually append them if needed.

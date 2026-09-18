import os

bib_file = "/home/marc/Documents/Agroscope/Article-P-desorption-kinetics/references.bib"

bib_entries = """
@Article{gustafssonModelingCompetitiveAnion2001,
  author       = {Gustafsson, Jon Petter},
  date         = {2001},
  journaltitle = {European Journal of Soil Science},
  title        = {Modeling Competitive Anion Adsorption on Oxide Minerals and an Allophane-containing Soil},
  doi          = {10.1046/j.1365-2389.2001.00407.x},
  volume       = {52},
  number       = {4},
  pages        = {639--653},
}

@Software{gustafssonVisualMINTEQ32014,
  author = {Gustafsson, Jon Petter},
  title = {Visual MINTEQ 3.1},
  date = {2014},
  url = {https://vminteq.lwr.kth.se/},
}
"""

with open(bib_file, "a") as f:
    f.write(bib_entries)

print("Appended Gustafsson citations to references.bib")

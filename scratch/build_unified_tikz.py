import os
import subprocess

tikz_styles = r"""
\usetikzlibrary{shapes, arrows.meta, positioning, decorations.pathmorphing, fit, backgrounds}
\tikzset{
    measured_pool/.style={rectangle, rounded corners, draw=black, very thick, fill=blue!10, text width=3.5cm, align=center, minimum height=2.2cm},
    solid_pool/.style={rectangle, rounded corners, draw=black, very thick, fill=orange!15, text width=3.5cm, align=center, minimum height=2.2cm},
    sink/.style={ellipse, draw=green!60!black, very thick, fill=green!10, text width=3.2cm, align=center, minimum height=1.5cm},
    agnostic_node/.style={rectangle, rounded corners, draw=red!80, very thick, fill=red!10, text width=3.5cm, align=center, minimum height=2.2cm},
    measured_arrow/.style={-{Stealth[scale=1.2]}, line width=1.5pt, draw=blue!80!black},
    standard_arrow/.style={-{Stealth[scale=1.2]}, thick, draw=black},
    agnostic_arrow/.style={-{Stealth[scale=1.2]}, line width=2pt, draw=red!80}
}
"""

def make_tex(content):
    return r"""\documentclass[tikz, border=2mm, transparent]{standalone}
\usepackage{helvet}
\renewcommand{\familydefault}{\sfdefault}
""" + tikz_styles + r"""
\begin{document}
\begin{tikzpicture}[node distance=3cm and 2cm]
""" + content + r"""
\end{tikzpicture}
\end{document}
"""

step_all = r"""
% Agnostic Approach (Left)
\node[agnostic_node] (agn_test) {\textbf{Static Soil Test} \\ (e.g. $P_{CO2}$ or $P_{AAE10}$)};
\node[sink, below=of agn_test] (agn_target) {\textbf{Empirical Target} \\ Fertilizer Multiplier};

\draw[agnostic_arrow] (agn_test.south) -- node[right, text=red!80, font=\bfseries] {Lookup Table} (agn_target.north);

% Mechanistic Approach (Right)
\node[solid_pool, right=4cm of agn_test] (mech_solid) {\textbf{Solid Phase P} \\ Quantity ($P_{AAE10}$)};
\node[measured_pool, right=5cm of mech_solid] (mech_soluble) {\textbf{Soluble P} \\ Intensity ($P_{CO2}$)};
\node[sink, below=of mech_soluble] (mech_plant) {\textbf{Plant Uptake} \\ \textbf{Yield} \\ \textbf{Tissue P concentration}};

\draw[measured_arrow] (mech_solid.20) -- node[above, font=\bfseries] {Desorption ($k$)} (mech_soluble.160);
\draw[standard_arrow] (mech_soluble.200) -- node[below, font=\bfseries] {Adsorption} (mech_solid.340);

\draw[<->, dashed, very thick, draw=purple] (mech_solid.north) -- ++(0,1) -| node[pos=0.25, above, font=\bfseries, text=purple] {Buffer Power ($b = dQ/dI$)} (mech_soluble.north);

\draw[measured_arrow, draw=green!60!black] (mech_soluble.south) -- node[right, align=left] {\textbf{Desorption Velocity ($v_0$)} \\ \textit{+ Diffusion Penalty ($1/b$)}} (mech_plant.north);

\draw[standard_arrow, dashed, line width=1.2pt, draw=orange!80!black] (mech_plant.west) to[out=180, in=270] node[left, font=\bfseries, align=center] {Critical $P_{CO2}$} (mech_solid.south);

% Grouping Boxes
\begin{scope}[on background layer]
    \node[draw=red!80, dashed, thick, fill=red!5, rounded corners, fit=(agn_test) (agn_target), label={[font=\bfseries\Large, text=red!80]above:Agnostic Approach}] {};
    \node[draw=green!60!black, dashed, thick, fill=green!5, rounded corners, fit=(mech_solid) (mech_soluble) (mech_plant) (current bounding box.north) (current bounding box.east), label={[font=\bfseries\Large, text=green!60!black]above:Mechanistic Approach}] {};
\end{scope}
"""

with open(f"scratch/step_all.tex", "w") as f:
    f.write(make_tex(step_all))

subprocess.run(["pdflatex", "step_all.tex"], cwd="scratch", stdout=subprocess.DEVNULL)
subprocess.run(["pdftocairo", "-svg", "step_all.pdf"], cwd="scratch")
print("Built step_all.svg")

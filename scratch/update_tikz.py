import re

tikz_styles = r"""
\usetikzlibrary{shapes, arrows.meta, positioning, decorations.pathmorphing}
\tikzset{
    measured_pool/.style={rectangle, rounded corners, draw=black, very thick, fill=blue!10, text width=3.5cm, align=center, minimum height=2.2cm},
    solid_pool/.style={rectangle, rounded corners, draw=black, very thick, fill=orange!15, text width=3.5cm, align=center, minimum height=2.2cm},
    unmeasured_pool/.style={rectangle, rounded corners, draw=gray, dashed, fill=gray!5, text width=2.5cm, align=center, minimum height=1.5cm, text=gray},
    sink/.style={ellipse, draw=green!60!black, very thick, fill=green!10, text width=3.2cm, align=center, minimum height=1.5cm},
    agnostic_node/.style={rectangle, rounded corners, draw=red!80, very thick, fill=red!10, text width=3.5cm, align=center, minimum height=2.2cm},
    measured_arrow/.style={-{Stealth[scale=1.2]}, line width=1.5pt, draw=blue!80!black},
    standard_arrow/.style={-{Stealth[scale=1.2]}, thick, draw=black},
    agnostic_arrow/.style={-{Stealth[scale=1.2]}, line width=2pt, draw=red!80},
    unmeasured_arrow/.style={dashed, -{Stealth[scale=1.2]}, thick, draw=gray}
}
"""

step1 = r"""
```{tikz}
%| echo: false
%| fig-align: center
""" + tikz_styles + r"""
\begin{tikzpicture}[node distance=4.5cm]
\useasboundingbox (-4,-2) rectangle (10, 5);

% Nodes
\node[agnostic_node] (soluble) {\textbf{Static Soil Test} \\ (e.g. $P_{CO2}$ or $P_{AAE10}$)};
\node[solid_pool, right=of soluble, opacity=0] (solid) {\textbf{Solid Phase P} \\ Quantity ($P_{AAE10}$)};
\node[sink, above=of soluble, yshift=1.5cm] (plant) {\textbf{Empirical Target} \\ Fertilizer Multiplier};

% Arrows
\draw[agnostic_arrow] (soluble.north) -- node[right, text=red!80, font=\bfseries] {Agnostic Black-Box} (plant.south);

\end{tikzpicture}
```
"""

step2 = r"""
```{tikz}
%| echo: false
%| fig-align: center
""" + tikz_styles + r"""
\begin{tikzpicture}[node distance=4.5cm]
\useasboundingbox (-4,-2) rectangle (10, 5);

% Nodes
\node[measured_pool] (soluble) {\textbf{Soluble P} \\ Intensity ($P_{CO2}$)};
\node[solid_pool, right=of soluble] (solid) {\textbf{Solid Phase P} \\ Quantity ($P_{AAE10}$)};
\node[sink, above=of soluble, yshift=1.5cm, opacity=0.2] (plant) {\textbf{Plant Uptake} \\ ($Y_{rel\_hist}$)};

% Arrows
\draw[measured_arrow] (solid.160) -- node[above, font=\bfseries] {Desorption ($k$)} (soluble.20);
\draw[standard_arrow] (soluble.340) -- node[below] {Adsorption} (solid.200);

\draw[<->, dashed, very thick, draw=purple] (solid.south) -- ++(0,-1) -| node[pos=0.25, below, font=\bfseries, text=purple] {Buffer Power ($b = dQ/dI$)} (soluble.south);

\end{tikzpicture}
```
"""

step3 = r"""
```{tikz}
%| echo: false
%| fig-align: center
""" + tikz_styles + r"""
\begin{tikzpicture}[node distance=4.5cm]
\useasboundingbox (-4,-2) rectangle (10, 5);

% Nodes
\node[measured_pool] (soluble) {\textbf{Soluble P} \\ Intensity ($P_{CO2}$)};
\node[solid_pool, right=of soluble] (solid) {\textbf{Solid Phase P} \\ Quantity ($P_{AAE10}$)};
\node[sink, above=of soluble, yshift=1.5cm] (plant) {\textbf{Plant Uptake} \\ ($Y_{rel\_hist}$)};

% Arrows
\draw[measured_arrow] (solid.160) -- node[above, font=\bfseries] {Desorption ($k$)} (soluble.20);
\draw[standard_arrow] (soluble.340) -- node[below] {Adsorption} (solid.200);

\draw[<->, dashed, very thick, draw=purple] (solid.south) -- ++(0,-1) -| node[pos=0.25, below, font=\bfseries, text=purple] {Buffer Power ($b = dQ/dI$)} (soluble.south);

\draw[measured_arrow, draw=green!60!black] (soluble.north) -- node[right, align=left] {\textbf{Desorption Velocity ($v_0$)} \\ \textit{+ Diffusion Penalty ($1/b$)}} (plant.south);

\end{tikzpicture}
```
"""

r_stack = f"""
::: {{.r-stack}}
::: {{.fragment .fade-out fragment-index=1}}
{step1}
:::
::: {{.fragment .fade-in-then-out fragment-index=1}}
{step2}
:::
::: {{.fragment .fade-in fragment-index=2}}
{step3}
:::
:::
"""

with open("presentation/index.qmd", "r") as f:
    content = f.read()

# Replace the mermaid chunk
mermaid_pattern = r'```{mermaid}.*?```'
match = re.search(mermaid_pattern, content, flags=re.DOTALL)
if match:
    content = content.replace(match.group(0), r_stack.strip())

with open("presentation/index.qmd", "w") as f:
    f.write(content)
print("Updated slide 2 to use TikZ r-stack")

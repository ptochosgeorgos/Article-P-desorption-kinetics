import re

with open('notebooks/qi_modelling1.qmd', 'r') as f:
    content = f.read()

# We need to replace the start parameter for m_yield_raw_aae
# Currently it is:
# start = c(0.1, unname(fixef(m_yield_raw_co2)[2:length(fixef(m_yield_raw_co2))])),
old_start_aae = "start = c(0.1, unname(fixef(m_yield_raw_co2)[2:length(fixef(m_yield_raw_co2))])),"

new_start_aae = """start = local({
        s <- fixef(m_yield_raw_co2)
        s[1:9] <- s[1:9] / 25
        s[17] <- s[17] * 25
        unname(s)
    }),"""

if old_start_aae in content:
    content = content.replace(old_start_aae, new_start_aae)
else:
    # Use regex
    content = re.sub(
        r'start = c\(0\.1.*?length\(fixef\(m_yield_raw_co2\)\)\]\)\),',
        new_start_aae,
        content,
        flags=re.DOTALL
    )

with open('notebooks/qi_modelling1.qmd', 'w') as f:
    f.write(content)

print("Fixed AAE10 starts!")

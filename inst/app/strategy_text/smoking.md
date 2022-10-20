Smoking is the biggest single cause of preventable death and ill-health within England. Reducing smoking prevalence through investment in smoking cessation services and public health interventions aimed at reducing take up will reduce admissions for a wide range of smoking attributable conditions. In addition historical measures such as national smoking bans will continue to have impact in future.

The 2018 Royal college of physicians report [Hiding in plain sight][rcp_hps] provides a list of conditions and their associated ICD10 codes that can be attributable to smoking. The model uses this list to identify spells in the model that could be avoided. 

[rcp_hps]: https://www.rcplondon.ac.uk/projects/outputs/hiding-plain-sight-treating-tobacco-dependency-nhs

Whilst most activity mitigation strategies identify all spells based on the specified SQL coding in this case the model only selects a proportion of spells based on the smoking attributable fraction (SAF) for that condition. As an example the AAF for cancer of the larynx for males is 43% therefore the model randomly selects 43% of spells meeting these criteria. The SAFs are also sourced from the above referenced document.
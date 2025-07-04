**Please Note**: By accessing this model you are acknowledging that you are aware of the implications of setting the model parameters, and have completed any relevant training.
If this is not the case, please contact your Model Relationship Manager, or contact [the data science team](mlcsu.su.datascience@nhs.net) before proceeding.

Changing the provider will update all of the information shown within the inputs application.
You will immediately see the map update showing the selected provider, as well as that trusts peers, as defined by the [Trust Peer Finder Tool](https://app.powerbi.com/view?r=eyJrIjoiMjdiOWQ4YTktNmNiNC00MmIwLThjNzktNWVmMmJmMzllNmViIiwidCI6IjUwZjYwNzFmLWJiZmUtNDAxYS04ODAzLTY3Mzc0OGU2MjllMiIsImMiOjh9).
The list of these peers is displayed below the map (collapsed by default).
Clicking on any of the points on the map will reveal the name of the provider.

Once you have chosen a provider, you can pick from one of the baseline years (currently only 2023/24), and then select the year that you want to use as the model horizon.
This defaults to 2041/42.

Finally, you must specify a scenario name - this will be how the results are identified in the inputs app.

To run a model with your given parameters, you must contact [the data science team](mlcsu.su.datascience@nhs.net), or (preferably), your Model Relationship Manager (if you have one), who will add you to the relevant Posit Connect Group to enable the 'Run Model' button to appear, provided you have completed the relevant training.

## Evidence Map

Much evidence exists in published literature of the efficacy of various methods to reduce or avoid acute hospital demand.
Although difficult to summarise, we have mapped this evidence in [an interactive app](https://connect.strategyunitwm.nhs.uk/nhp_evidence_map/) to provide an overview, categorised by setting, evidence type, outcomes and effect.
This map is provided to enable exploration of the current evidence landscape, identify gaps in knowledge, and allow searching of evidence by a range of parameters.

## Advanced Options

By default, these are hidden.
There are two options that can be adjusted here:

- the random seed used for the model run. This defaults to a random value each time, but can be set to a specific value to repeat a prior model run
- the number of iterations to use within the Monte-Carlo simulation. This defaults to 256 runs, but can be set to 512 or 1024 runs

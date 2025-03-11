Changing the provider will update all of the information shown within the inputs
application. You will immediately see the map update showing the selected
provider, as well as that trusts peers, as defined by the
[Trust Peer Finder Tool][1]. The list of these peers is displayed below the
map (collapsed by default). Clicking on any of the points on the map will reveal
the name of the provider.

[1]: https://app.powerbi.com/view?r=eyJrIjoiMjdiOWQ4YTktNmNiNC00MmIwLThjNzktNWVmMmJmMzllNmViIiwidCI6IjUwZjYwNzFmLWJiZmUtNDAxYS04ODAzLTY3Mzc0OGU2MjllMiIsImMiOjh9

Once you have chosen a provider, you can pick from one of the baseline years,
and then select the year that you want to use as the model horizon. This
defaults to 15 years from the baseline.

Finally, you must specify a scenario name - this will be how the results are
identified in the inputs app.

## Advanced Options

By default, these are hidden. There are two options that can be adjusted here:

* The random seed used for the model run. This defaults to a random value each
time, but can be set to a specific value to repeat a prior model run
* The number of iterations to use within the monte-carlo simulation. This
defaults to 256 runs, but can be set to 512 or 1024 runs.

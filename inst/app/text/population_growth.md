These inputs reflects the probability that each population projection variant
will be selected in each model iteration (for monte carlo simulations). The
population projection variants are from ONS published 2018-based national
population projections (NPP) and sub-national population projections (SNPP)[1].

[1]: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/localauthoritiesinenglandz1

These are used in combination to create LA-level principal projections plus 4
LA level and 17 national-level variant projections (reflecting different
assumptions around migration, fertility etc.).  Where only national-level
variant projections are available, LA-level variant projections are imputed, by
applying ratios of the national-level variants to local populations. In total
there are 19 variants plus 1 principal projection, i.e. 20 options in total.

These projections are used to adjust baseline activity to give an estimate of
activity in the model horizon year by taking the ratio (by age and sex) of the
population in the model horizon year divided by the the population in the
baseline year.

For example, if there are projected to be 1200 5 year old males in the model
horizon year, and there are 1000 in the baseline, then we will have a growth
of 1.2 (or 20%).

## Using the sliders

The sum of all of the sliders equals 100%. There is one slider, the principal
projection, which cannot be changed directly by the user. It is set by summing
all of the other sliders, and then "filling" the remainder value to reach 100%.

If the user sets a value for one of the sliders which causes the sum of the
values to exceed 100%, the other sliders are gradually reduced equally in order
to maintain the condition that the sum cannot exceed 100%.

## Data source

Probabilities assigned to each projection variant are currently user assumptions
only. Initial intention was to devise and agree a probability distribution
across the principal projection and 19 variants, allowing us to incorporate a
range of projections and uncertainties using Monte Carlo simulation. However,
ONS state that the method used to produce the projections does not enable
statements of probability to be attached to them, or for confidence intervals to
be ascribed to the variant projections because the variants are based on
different demographic scenarios - they are illustrations of alternative
scenarios and are not quantified measures of uncertainty. Therefore this
approach would not be in line with ONS guidance, and no robust probabilities are
available currently (though potentially we could look to elicit some from
experts in future).

We therefore propose to run variants as individual scenarios, with Principal
Projection and High/Low population variants as standard to give a sense of the
range and sensitivities. 

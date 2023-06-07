The baseline data that is used in the model is the "Hospital Episodes
Statistics" ([HES][1]) data, which is submitted by providers via the Secondary
Users Service (SUS) data sets.

[1]: https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/hospital-episode-statistics

This dataset contains all of the hospitals activity for a given year, so should
be a complete and accurate record for our baseline.

However, there are circumstances where activity that was undertaken was not
recorded and submitted via SUS, or there was a gap in activity due to some
unusual or unexpected reason (that isn't already acounted for via the
expat/repat parameters).
Here users can specify one type of demand-supply imbalance at baseline, relating
to waiting times for elective care at baseline. Waiting times for elective care
are subject to several national targets. Patients waiting for care are placed
on a patient tracking list (PTL) and are removed once their treatment reaches a
certain stage. If the number of patients being referred to a trust and
therefore being added to a PTL is larger in any given period than the number
being removed, then waiting times will tend to increase and vice versa.

Given that these effects are cumulative, a trust might aim in the long term for
its PTL additions and removals to be equivalent. If this is not the case at
baseline, then the user may wish to specify changes in the level of elective
activity at baseline. Users can set model parameters to adjust the level of
elective inpatient activity and outpatient activity by specialty. The parameter
that is passed to the model indicates the number of additional (or fewer) units
of relevant activity that would have been required to balance demand and supply
in the baseline period.

In the model we take the ratio of the changed activity / baseline activity.
This value is used as the parameter "lambda" from a random Poisson distribution.

The values defualt to 0, i.e. no change. A positive value will indicate an
increase to waiting lists (generating more activity in the model), and a
negative value will indicate a decrease to waiting lists (generating less
activity in the model).

The inpatients values relate to the amount of elective waiting list admissions
([admission method = '11'][1]), and the outpatients relates to the amount of all
outpatient attendances for that treatment function.

[1]: https://www.datadictionary.nhs.uk/attributes/admission_method.html

The values default to the actual amount of activity for that treatment function
in the baseline.

## Data source

The values shown here reflect the average monthly change to the size of the
waiting list per treatment function, for the period Jan 2015 - Jan 2019. It is
calculated based on published [NHS Referral To Treatment Statistics data][2] on
Waiting Lists (monthly at provider level); It reflects Elective admissions only. 

[2]: https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/

## Calculation approach

Total Number of Incomplete Pathways (taken as size of Waiting List) is extracted
for each Treatment Function for each cohort 4 trust, for each month between Jan
2015 and Dec 2019 from NHS published statistics on RTT waiting times. For each
Treatment Function/Trust, month to month change to size of waiting list is
calculated. Following that, outliers are identified and removed, and an average
monthly change is produced. This average monthly change approximates the
demand/supply imbalance that is applied to the model baseline - i.e. if waiting
lists are increasing, this increase is added to baseline activity, approximating
keeping waiting lists stable.

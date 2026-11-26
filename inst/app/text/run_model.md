Once you have set all of the parameters you want to set, you can submit them to the model to be run.

Before that, you can view your parameter selections by expanding the 'View Params' box on this page.
The parameters are shown in the [JSON][json] format that will be submitted to the model engine.
You can download the JSON file if you click the 'Download Params' button.

[json]: https://en.wikipedia.org/wiki/JSON

When you are ready:

1. Click the 'Submit Model Run' button in the 'Model Run' box. The rest of the app will be disabled to prevent multiple submissions.
1. Text beneath the button will change to 'Please Wait...'.
1. You'll be autoforwarded to the OpenPlan Demand Model Run Progress App. You can click the 'View Progress' link that appears under the button if you aren't forwarded within a few seconds.
1. In the progress app, you'll see the text 'checking...' followed by 'Model Running [Inpatients: 0/256 (0.0%)]', which tells you how many model runs have been completed for each activity type (inpatients, outpatients and A&E). There are also overall progress for each activity type. The app will refresh every few seconds to give you an update. You can expand the model-run metadata box for a reminder of the scenario being submitted.
1. When the model run is complete, you'll get be autoforwarded to the OpenPlan hospital demand model outputs app, which will be pre-loaded with results for your scenario. You can click the 'View Results' button if you are not forwarded within a few seconds.

Note that the process may time out if the model is taking slightly longer to run than usual.
A timeout does not mean that the run has been stopped.

Once you have set all of the parameters, you can submit these to the model
to be run.

Pressing the submit button will disable the rest of the application, preventing
you from submitting that set of parameters multiple times.

You may download the parameters, which can be reuploaded at a later date on the
home page of the application. This can be useful if you have not yet finished
setting the model parameters and wish to come back to it later.

## Running the model

When you submit the model to run you will see a status message appear. The
messages that you will see are:

* "Please wait" while the model run is being submitted,
* "Submitted model run" once the model has been submitted, but the model engine
has not yet started
* "Model running" once the model engine has started
* "Completed: View Outputs" once the model engine has finished. Clicking the
'View Outputs' link will take you directly to the results

The app may time out if the model is taking slightly longer to run than usual. A timeout in the app does not mean that the run has been stopped. Please wait a short while and go to the NHP outputs app to confirm success and to retrieve the results of the run. 

If anything goes wrong running the model then "Error" will appear in the status.

## View params

The complete model parameters are available to view as [JSON][1] in the right
hand panel. This is the file that will be submitted to the model engine. This is
primarly kept in for development purposes and may be removed in future releases.

[1]: https://en.wikipedia.org/wiki/JSON

## Resubmission

This is a resubmission. In this version, I have:

* Changed the title of the package in the description, removing the redundant "An R package";

* Added references to the description file;

* Added \value to bd_decode.Rd;

* Added the argument "verbose" to functions learn_DAG() and get_causaleffect() to make it easy to suppress messages to the console;

* Included on.exit() within functions get_diagnostics() and summary.bcdag() to restore the user's options once they have been modified by the functions;

* Added a chunk of code at the end of each vignette .Rmd file to restore the user's previous options;

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs. 

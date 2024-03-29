# Bpred App

## Version: 23.12.1

### Bug Fixes
- _Import of models from Pandora_: 
  - an error message occurred when trying to load a model from pandora.
  - fix: adding the missing download of the zip file from the url before unpacking the zip

## Version: 23.12.0

### New Features
- _Import of models_: display of "About" information that is associated to a selected Pandora 
  Repository

## Version 23.09.0

### New Features
- _Import of models_:
  - option to import models from Pandora platform

## Version 23.08.0

### Bug Fixes
- updates error message (#27)

## Version 23.07.1

### New Features
- show Bayesian R-squared in formula overview (following: https://avehtari.github.io/bayes_R2/bayes_R2.html)

## Version 23.07.0

### Bug Fixes
- fix bug with the header of matrix and the update of the matrix (#22)
- use most recent version of shinyMatrix

## Version 23.03.1

### Bug fixes
- add remote package to enable the _Import Data_ module

## Version 23.02.2

### Updates
- catch errors and(!) warnings within a pop up

## Version 23.02.1

### New Features
- the _Import Data_ module is now imported from the new package DataTools 
  - additionally to file import, now import from _URL_ or from _Pandora Platform_ is possible
  - all redundant code was removed
  - using "file" as default source in _Import Data_

## Version 23.01.2

### New Features
- fixed sidebars with auto scroll in all tabs (iso-app #4)

### Updates
- path to remote models was added to the ReadMe 

## Version 23.01.1

### Updates
- remote models are loaded from the github folder `inst/app/predefinedModels` of the respective 
repository (#1)
  - if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version

## Version 22.11.2

### New Features
- _Impute missings_ as option in "Estimates" tab.

## Version 22.11.1

### New Features
- save inputs when downloading a model (#1)
- update all input fields after uploading a model (#1)

### Updates
- also update table of measures after upload of a model (#13)

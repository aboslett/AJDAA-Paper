This folder contains most scripts used for our paper published in the American Journal of Drug and Alcohol Abuse.

Here is what each script does:
0-initialization-file
* Loads dependencies and sets up directory
1-set-up-death-database
* Creates annual databases of drug overdoses
* Adds characteristics
* Could be broadly useful to others using NCHS data
2-add-ccs-ucods-covariates-to-database
* Adds common contributing causes to database by year
3-calculate-new-estimates-of-opioid-overdoses-...
* Calculates estimates of opioid overdoses with corrections for unidentified drug overdoses
4-calculate-accuracy-statistics-balanced-obs...
* Generates estimates of total predictive accuracy for models with equal #s of opioid and non-opioid overdoses in obs. set
5-calculate-accuracy-statistics-unbalanced-...
* Same as 4 but does not balance obs. set (baseline results in paper)
6-calculate-accuracy-statistics-balanced-obs...
* Randomizes seed and creates a figure showing distribution of accuracy estimates across seeds (i.e., is this an effect of a random sample?)
7-create-heatmap-of-opioid-overdose-changes...
* Creates figure showing change in opioid overdoses from 2017-2018 with different levels of opioid involvement in unidentified drug overdoses
8-calculate-roc-curves-balanced-obs-set
* Calculates receivor operating curves across candidate models
9-export-list-of-common-contributing-causes
* Creates a list of the most common contributing causes by year
10-create-figures-of-trends-in-drug-overdoses-and-chars
* Creates figures showing trends in drug overdoses, opioid involvement, and the % of unidentified drug overdoses across time
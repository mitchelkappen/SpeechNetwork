# Code accompanying manuscript titled:
## "Speech as an Indicator for Psychosocial Stress: a Network Analytic Approach"

This repository consists of four folders

1. Data
2. Code
3. Plots
4. Supplemental Materials

Code folder consists of 'Analysis code' - Python and R scripts used to extract and model features and 'BootFiles' - generated in R script during bootstrapping.

## Data Analysis
All data analysis codes are found in ```/Code/Analysis```.
We used R (with RStudio) and Python 3 (with JupyterLab).
Used version, packages, and toolboxes can be found in the supplemental materials.
### Feature Extraction - Jupyter
As a first step features were extracted from voice fragments recorded during unstressed (pre-stressor) and stressed (post-stressor) parts of the experiment.
Features were extracted using OpenSmile version 2.3.0 (Eyben et al., 2010) and the GeMAPS configuration (Eyben et al., 2015).


### Data
Raw audio files are not openly accessible due to its sensitivity with regards to privacy.
However, ```/Data/Raw``` contains two randomly selected participant folders containing :
1) appData.json - containing the behavioural data collected during the experiment
2) chest.csv - containing the collected ECG data
3) eda.csv - containing the collected EDA data
4) unstressed.wav - read-out-loud audio collected pre-stress induction
5) stressed.wav - read-out-loud audio collected post-stress induction.
```
See "Code/Analysis/OpenSMILE_featureExtraction.ipynb" for feature extraction script
```
Output files from this script can be found in ```/Data/```
- ```geMapsFeatures_pre.csv``` - Contains used speech features pre-stressor
- ```geMapsFeatures_post.csv``` - Contains used speech features post-stressor
- ```speechFeatures_geMaps_PrePostDelta.csv``` - Both aforementioned files merged together plus delta (change) scores added (post minus pre values)

This data is then merged with the other data (questionnaires - affect scores - demographics - EDA measures) and saved as ```completeData.csv```.

You will also find a file called ```voiced_unvoiced.csv```, this contains the number of frames for each participant as well as the number of voiced and unvoiced frames.
It is categorized per participant and per experimental phase, where ```unstressed.wav``` corresponds to the audio collected pre-stressor and ```stressed.wav``` corresponds to the audio collected post-stressor.

### R Script
All other data manipulations and analysis as described in the methods and results sections of our manuscript are done in R.
```
See "Code/Analysis/Script_08-06-2021.R"
```
#### Manipulation Check
The first part of this code is the manipulation check - whether the stress induction was successful.
A check for normality was performed (Shapiro-Wilks) and (G)LMMs to conclude a successful stress induction
In addition a correlation (Pearson) was performed
#### Network analysis
Network analysis are described in the manuscript in the same order as they are executed in the script.
1. Network analysis resting state speech parameters
2. Network analysis stress state speech parameters
3. Network analysis stress reactivity - consists of change/delta scores for selected speech parameters and negative affect (NA)
#### Supplemental
At the bottom of each section of code each of the relevant supplemental materials for that section is generated.

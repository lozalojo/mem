# The Moving Epidemic Method R Package

[![Travis-CI Build Status](https://travis-ci.org/lozalojo/mem.svg?branch=master)](https://travis-ci.org/lozalojo/mem)
[![Coverage Status](https://img.shields.io/codecov/c/github/lozalojo/mem/master.svg)](https://codecov.io/github/lozalojo/mem?branch=master)
[![DOI](https://zenodo.org/badge/47120918.svg)](https://zenodo.org/badge/latestdoi/47120918)
[![monthly](http://cranlogs.r-pkg.org/badges/mem)](https://www.rpackages.io/package/mem)
[![total](http://cranlogs.r-pkg.org/badges/grand-total/mem)](https://www.rpackages.io/package/mem)
[![CRAN](http://www.r-pkg.org/badges/version/mem?color=009999)](https://cran.r-project.org/package=mem)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

## Overview

The Moving Epidemics Method *MEM* is a tool developed in the *Health Sentinel Network of Castilla y León (Spain)* to help in the routine influenza surveillance in health systems. It gives a better understanding of the annual influenza epidemics and allows the weekly assessment of the epidemic status and intensity.

Although in its conception it was originally created to be used with influenza data and health sentinel networks, MEM has been tested with different respiratory infectious diseases and surveillance systems so nowadays it could be used with any parameter which present a seasonal accumulation of cases that can be considered an epidemic.

MEM development started in 2001 and the first record appeared in 2003 in the *Options for the Control of Influenza V*.
It was presented to the baselines working group of the *European Influenza Surveillance Scheme (EISS)* in the *12th EISS Annual Meeting (Malaga, Spain, 2007)*, with whom started a collaboration that continued when, in 2008, was established the European Influenza Surveillance Network.

In 2009 MEM is referenced in an official European document: the *Who European guidance for influenza surveillance in humans*. A year later MEM was implemented in *The European Surveillance System (TESSy)*, of the *European Centre for Disease Prevention and Control (ECDC)*, and in 2012, after a year piloting, in the *EuroFlu regional influenza surveillance* platform, of the *World Health Organization Regional Office for Europe (WHO-E)*.

As a result of the collaboration with *ECDC* and *WHO-E*, two papers have been published, one related to the establishment of epidemic thresholds and other to the comparison of intensity levels in Europe.

## Installation

To install the lastest stable version from the official R repositories (*CRAN*):

```
# install the mem CRAN version
install.packages("mem")
```

To install the lastest development version of *mem*.


```
if(!require("devtools")) install.packages("devtools")
library("devtools")
# install the memapp development version from GitHub
install_github("lozalojo/mem")
```

## Usage

```
# load the library
library("mem")

# run the help
help("mem")
```

## References

Vega T, Lozano JE, Ortiz de Lejarazu R, Gutierrez Perez M. Modelling influenza epidemic—can we detect the beginning and predict the intensity and duration? Int Congr Ser. 2004 Jun;1263:281–3. 

Vega T, Lozano JE, Meerhoff T, Snacken R, Mott J, Ortiz de Lejarazu R, et al. Influenza surveillance in Europe: establishing epidemic thresholds by the moving epidemic method. Influenza Other Respir Viruses. 2013 Jul;7(4):546–58. DOI:10.1111/j.1750-2659.2012.00422.x.

Vega T, Lozano JE, Meerhoff T, Snacken R, Beaute J, Jorgensen P, et al. Influenza surveillance in Europe: comparing intensity levels calculated using the moving epidemic method. Influenza Other Respir Viruses. 2015 Sep;9(5):234–46. DOI:10.1111/irv.12330.

Lozano JE. lozalojo/mem: Second release of the MEM R library. Zenodo [Internet]. [cited 2017 Feb 1]; Available from: https://zenodo.org/record/165983. [![DOI](https://zenodo.org/badge/47120918.svg)](https://zenodo.org/badge/latestdoi/47120918)

Lozano JE. lozalojo/memapp: Second release of the MEM Shiny Web Application R package. Zenodo [Internet]. [cited 2018 Feb 15]; Available from: https://zenodo.org/record/1173518. [![DOI](https://zenodo.org/badge/90709196.svg)](https://zenodo.org/badge/latestdoi/90709196)

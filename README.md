# mem
Moving Epidemics Method R Package

This is the R package of the Moving Epidemics Method

Vega T., Lozano J.E. (2004) Modelling influenza epidemic - can we detect the beginning 
and predict the intensity and duration? International Congress Series 1263 (2004) 
281-283.

Vega T., Lozano J.E. (2012) Influenza surveillance in Europe: establishing epidemic 
thresholds by the Moving Epidemic Method. Influenza and Other Respiratory Viruses, 
DOI:10.1111/j.1750-2659.2012.00422.x.

Code programmed by Jose E. Lozano

INSTALLATION INSTRUCTIONS:

# in case you dont have devtools installed, install it
install.packages(devtools)
library(devtools)

# detach your old mem package (if it is currently loaded)
detach("package:mem")
# remove your old mem package (if it is already installed)
remove.packages("mem")
# install the new package from github
install_github("lozalojo/mem")



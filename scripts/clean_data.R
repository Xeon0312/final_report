library(cesR)
library(labelled)
library(tidyverse)

# call 2019 CES online survey
get_ces("ces2019_web")
# convert values to factor type
ces2019_web <- to_factor(ces2019_web)
write_csv(ces2019_web, "ces2019_web.csv")

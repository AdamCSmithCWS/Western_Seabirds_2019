# Western_Seabirds_2019

This mess is the most recent analysis of the West Coast Seabird burrow-count data from the long-term monitoring plots.

The analysis takes counts of burrows from permanent monitoring plots on major seabird colonies in Western Canada, as well as single point-in-time estimates of the total colony size in at least one year, to generate population trajectories for each colony and the overall monitored population (sum of estiamtes across colonies).
The time-series is modeled using a hierarchical GAM structure, where the shape of the trajectory is estimated as a random effect that's allowed to vary among colonies, but is assumed consistent among plots within a colony.

# Apologies: 
It pre-dates my ongoing exploration of the Tidyverse, so it may require an overall re-write.


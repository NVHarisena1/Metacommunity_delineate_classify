#Introduction
The repository contains R code to create spatial habitat networks across time and assess metacommunity definitions based on this historical networks. This refers to the work done in paper:
  Harisena, N. V., Grêt-Regamey, A., & Van Strien, M. J. (2024). Identification of metacommunities in bioregions with historical habitat networks. Ecology and Evolution, 14(8), 1–13. https://doi.org/10.1002/ece3.70076

##Code details
###1&2.wetland_timesteps and wetland_correction
    Aggregation of wetland cover for different timesteps and its correction to not include emergence of new wetladns over time, see paper for more details. 
###3.Create Networks and trace ancestry- 
    a. Create habitat networks using "Graph4lg" package for each year based on wetland landcover maps. Also identify network components i.e. disconnected groups a.k.a. metacommunity extents. The functions in "functions_forgraph4lg" must be run to override certain issues in the graph4lg package. 
    b. Identifies network precedants based on habitat patch overlaps ny creating text chains of ID's of patches as they overlap over time. 
###4.Create_adjacency_matrices_for_different_years
    All past distances are attributed to patche ID's in the present based on spatial overlap of patches overtime.
###5_Results_metacommunity_delineation_fig3
    Calculate beta diversity (jaccard distance) of odonate species per patch pair and create boxplot of "within" and "between" beta diversities for metacommunity definitions over time. Also calculate Kappa statistic and Anova for each boxplot pair. 
###6.Gamma_metacommunitysize
    Calculate Gamma diversity relationships to metacommunity-size parameters

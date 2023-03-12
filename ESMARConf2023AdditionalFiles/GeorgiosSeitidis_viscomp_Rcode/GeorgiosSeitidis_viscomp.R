#########################################################
#             Install and load packages
#########################################################

install.packages("viscomp")
install.packages("openxlsx")
install.packages("netmeta")
install.packages("ggplot2")

library(viscomp)
library(openxlsx)
library(netmeta)
library(ggplot2)

#########################################################
#                     Load data
#########################################################

# Body Mass Index outcome (BMI)
bmi <- read.xlsx("BMI.xlsx") 

# High-density lipoprotein (HDL) cholesterol outcome
hdl <- read.xlsx("HDL.xlsx")

# Waist size
waist_size <- read.xlsx("Waist size.xlsx")


#########################################################
#            Perform Network Meta-Analysis
#########################################################

# BMI NMA model (Assumed harmful outcome)
m <- netmeta(TE = TE, seTE = seTE, treat1 = treat1, treat2 = treat2, studlab = studlab,
                 data = bmi, small.values = "good", reference.group = "A") 


# Network plot
netgraph(m, cex = 0.6, rotate = 50, multiarm = F, scale = 0.9)


#------------------------------------------------------------------------------------------------------------------------------------
#                                        Visualize multi-component NMA results 
#------------------------------------------------------------------------------------------------------------------------------------
 

#########################################################
#           Components Descriptive Analysis
#########################################################
 
?compdesc

compdesc(m)

# Use fraction values instead of percentages
compdesc(m, percentage = FALSE) 

#########################################################
#              Components Network Graph
#########################################################

?compGraph

compGraph(m)

# Exclude nodes A and D, and visualize the 6 most frequent component combinations
compGraph(m, mostF = 6, excl = c("A", "D"), title = "")

#########################################################
#  Leaving One Component Combination Out Scatter plot
#########################################################

?loccos

# Visualize the set of interventions that differ by one component
loccos(m) 

# Visualize the set of interventions that differ by the component "C"
loccos(m, combination = "C") 

# Change the color of the histogram
loccos(m, combination = "C", histogram.color = "red") 

# Exclude histogram
loccos(m, combination = " C", histogram = FALSE) 

# Visualize the set of interventions that differ by the component combination "B + C"
loccos(m, combination = "B + C", histogram = FALSE) 

# The spacing between the components does not affect the results
loccos(m, combination = "B +C", histogram = FALSE)
loccos(m, combination = "B+C", histogram = FALSE)

# Use z-values instead of NMA relative effects
loccos(m, combination = "C", histogram = FALSE, z_value = TRUE)

#########################################################
#                   Waterfall plot
#########################################################

?watercomp

# Visualize the set of interventions that differ by one component
watercomp(m)

# Visualize the set of interventions that differ by the component "C"
watercomp(m, combination = "C") + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 90),
                                                 axis.text =  ggplot2::element_text(size = 15),
                                                 axis.title =  ggplot2::element_text(size = 15))

# Visualize the set of interventions that differ by the component combinations"B + C"
watercomp(m, combination = "B + C") + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 90),
                                                     axis.text =  ggplot2::element_text(size = 15),
                                                     axis.title =  ggplot2::element_text(size = 15))

# Use z-values instead of NMA treatment effects
watercomp(m, combination = "B + C", z_value = TRUE) + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 90),
                                                                  axis.text =  ggplot2::element_text(size = 15),
                                                                  axis.title =  ggplot2::element_text(size = 15))

#########################################################
#                Components Heat Plot
#########################################################

?heatcomp

# Components Heat plot
heatcomp(m)

# Use the median instead of the mean as a summary measure
heatcomp(m, median = FALSE)

# Use z-values instead of NMA relative effects
heatcomp(m, z_value = TRUE)

# Remove component combinations frequency
heatcomp(m, z_value = TRUE, freq = FALSE)

#########################################################
#    Specific Component Combination violin plots
#########################################################

?specc

# Violin plot for each component
specc(m) + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13),
                            axis.text =  ggplot2::element_text(size = 15),
                            axis.title =  ggplot2::element_text(size = 15))

# Violin plot for the interventions that include components: 
# 1) B 
# 2) E 
# 3) E and B
specc(m, combination = c("B", "E", "E + B")) + ylab("Intervention effect")

# Violin plots based on the number of components
specc(m, components_number = TRUE) + ylab("Intervention effect")

# Grouping violin based on the interventions that include:
# 1 component
# 1 to 3 components
# 4 to 5 components
# 6 or more components
specc(m, components_number = TRUE, groups = c("1", "1-3", "4-5", "6+")) + ylab("Intervention effect")

# Use z-values instead of NMA treatment effects
specc(m, components_number = TRUE, z_value = TRUE, groups = c("1", "1-3", "4-5", "6+")) + ylab("Intervention effect")

#########################################################
#              Components Density Plot
#########################################################

?denscomp

# Density plot for the interventions that:
# 1) include component B
# 2) do not include component B
denscomp(m, combination = "B")

# Density plot for the interventions that:
# 1) include components B, C and I
# 2) do not include components B, C and I
denscomp(m, combination = "C + B + I")

# Density plot for the interventions that:
# 1) include components E and B
# 2) include component E
# 3) include component C
denscomp(m, combination = c("E + B", "E", "C"))

# Use violins instead of density plots
denscomp(m, combination = c("E + B", "E", "C"), violin = TRUE)

# Use z-values instead of NMA relative effects
denscomp(m, combination = "B", z_value = TRUE)

#########################################################
#             Components Rank Heat Plot
#########################################################
?rankheatplot

# Multiple outcomes

# NMA model for the High-density lipoprotein cholesterol outcome (Assumed as beneficial outcome)
m_hdl <- netmeta(TE = TE, seTE = seTE, treat1 = treat1, treat2 = treat2, studlab = studlab, 
             data = hdl, small.values = "good", reference.group = "A") 


# NMA model for the waist size outcome (Assumed harmful outcome)
m_ws <- netmeta(TE = TE, seTE = seTE, treat1 = treat1, treat2 = treat2, studlab = studlab, 
                data = waist_size, small.values = "good", reference.group = "A") 


# Rank heat plot
rankheatplot(list(m, m_hdl, m_ws), 
             outcomeNames = c("BMI", "HDL", "Waist Size"), 
             cex_values = 1.2, 
             cex_outcomes = 1, 
             cex_components = 1)

# Use the mean as summary measure instead of the median
rankheatplot(list(m, m_hdl, m_ws), 
             median = FALSE,
             outcomeNames = c("BMI", "HDL", "Waist Size"), 
             cex_values = 1.2, 
             cex_outcomes = 1, 
             cex_components = 1)

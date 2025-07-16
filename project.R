setwd('/home/leo/analytical_palaeobiology_project/')
df = read.csv('mass_extinction_divDyn.csv')
df_ammonEx = read.csv('mass_extinction_divDyn_ammonites_not_predators.csv')
df_triloEx = read.csv('mass_extinction_divDyn_trilobites_not_predators.csv')

##### log odds analysis #### 

logOdds = function(a, b) {
  log(sum(a, na.rm = TRUE) / sum(b, na.rm = TRUE))
}


logOdds(df[df$diet == "Carnivore" & df$mass_extinction == TRUE, ]$extPC, df[df$diet == "Non-Carnivore" & df$mass_extinction == TRUE, ]$extPC)
logOdds(df[df$diet == "Carnivore" & df$mass_extinction == FALSE, ]$extPC, df[df$diet == "Non-Carnivore" & df$mass_extinction == FALSE, ]$extPC)


########

carniCol = "brown1"
nonCarniCol = "black"

full_tsplot <- function(x = NULL,
                        ys = NULL,
                        cols = c(carniCol, nonCarniCol),
                        labels = c("Carnivores", "Other"),
                        xlim = 4:95,
                        ylim = c(0, 1),
                        ylab = "Diversity Proportion",
                        boxes = "sys",
                        shading = "sys",
                        event_lines = c(66, 201, 252, 372, 445),
                        leg = TRUE,
                        legend_pos = "topleft",
                        legend_inset = c(0.05, 0.05),
                        legend_cex = 1.3,
                        tit = "Diversity") {
  # Plot the tsplot background
  tsplot(
    stages,
    boxes = boxes,
    shading = shading,
    xlim = xlim,
    ylim = ylim,
    ylab = ylab
  )
  
  # Add the lines
  for (i in seq_along(ys)) {
    lines(x, ys[[i]], col = cols[i], lwd = 2)
  }
  
  # Add vertical event lines
  abline(v = event_lines,
         col = "gray20",
         lty = 2)
  
  # add horizontal line
  abline(h = 0)
  
  
  # Add legend
  if (leg) {
    legend(
      legend_pos,
      bg = "white",
      legend = labels,
      title = "Diet",
      col = cols,
      lwd = 2,
      inset = legend_inset,
      cex = legend_cex
    )
  }
  
  # Add title
  title(tit)
}



###### diversity rates #####
full_tsplot(
  x = df[df$diet == "Both", ]$mid,
  ys = list(df[df$diet == 'Carnivore', ]$divSIB, df[df$diet == 'Non-Carnivore', ]$divSIB),
  tit = "Diversity",
  ylab = "SIB Diversity",
  ylim = c(0, max(
    df[df$diet == "Carnivore", ]$divSIB, df[df$diet == "Non-Carnivore", ]$divSIB, na.rm = TRUE
  ))
)

# Total SIB diversity compared with carni diversity
prop_tsplot = function(proportion, tit = "Relative Makeup of Diets") {
  full_tsplot(leg = FALSE, tit = tit, ylab = "SIB proportion")
  y_poly = c(proportion, rep(0, length(proportion)))
  x = c(df[df$diet == "Both", ]$mid, rev(df[df$diet == "Both", ]$mid))
  polygon(x, y_poly, col = rgb(1, 0, 0, 0.7), border = NA)
  
  y_poly_inv = c(proportion, rep(1, length(proportion)))
  polygon(x, y_poly_inv, col = rgb(0, 0, 0, 0.7), border = NA)
  abline(v = c(66, 201, 252, 372, 445), col = "white")
  legend(
    "topright",
    bg = "white",
    legend = c("Carnivore", "Non-Carnivore"),
    title = "Diet",
    col = c(rgb(1, 0, 0), rgb(0, 0, 0)),
    lwd = 2,
    inset = c(0.05, 0.05),
    cex = 1.3,
  )
  
}
total = df[df$diet=="Both",]$divSIB
carni = df[df$diet=="Carnivore",]$divSIB
proportion = carni / total

prop_tsplot(proportion, tit = "Porportion of Diversity by Diet")


# exclude ammonites
total = df[df$diet == "Both", ]$divSIB
carni = carniAmmonExDiv$divSIB
proportion = carni / total
prop_tsplot(proportion, tit = "Proportion of Diversity by Diet (Ammonites not Carnivore)")

# exclude trilobites
total = df[df$diet == "Both", ]$divSIB
carni = df_triloEx[df_triloEx$diet == "Carnivore", ]$divSIB
proportion = carni / total
prop_tsplot(proportion, tit = "Proportion of Diversity by Diet (Trilobites not Carnivore)")


##### total extinction rates
full_tsplot(
  x = df[df$diet == "Both", ]$mid,
  ys = list(df[df$diet == "Carnivore", ]$extPC, df[df$diet == "Non-Carnivore", ]$extPC),
  tit = "Extinctions",
  ylab = "Foote Metric",
  ylim = c(0, max(
    max(carniAmmonExDiv$extPC, na.rm = TRUE),
    max(noncarniAmmonExDiv$extPC, na.rm = TRUE),
    max(df_triloEx[df_triloEx$diet == "Carnivore", ]$extPC, na.rm = TRUE),
    max(nondf_triloEx[df_triloEx$diet == "Carnivore", ]$extPC, na.rm = TRUE)
  ))
)
# add ammonite lines
lines(
  x = df[df$diet == "Both", ]$mid,
  y = carniAmmonExDiv$extPC,
  lwd = 2,
  col = carniCol,
  type = "l",
  lty = 2
)
lines(
  x = df[df$diet == "Both", ]$mid,
  y = noncarniAmmonExDiv$extPC,
  lwd = 2,
  col = nonCarniCol,
  type = "l",
  lty = 2
)
legend(
  "topright",
  bg = "white",
  legend = c("Default", "Ammonites not carnivore"),
  lwd = 2,
  lty = c(1, 2),
  inset = c(0.05, 0.05),
  cex = 1.3
)

# add trilobite lines
lines(
  x = df[df$diet == "Both", ]$mid,
  y = df_triloEx[df_triloEx$diet == "Carnivore", ]$extPC,
  lwd = 2,
  col = carniCol,
  type = "l",
  lty = 3
)
lines(
  x = df[df$diet == "Both", ]$mid,
  y = nondf_triloEx[df_triloEx$diet == "Carnivore", ]$extPC,
  lwd = 2,
  col = nonCarniCol,
  type = "l",
  lty = 3
)
legend(
  "topright",
  bg = "white",
  legend = c("Default", "Ammonites not carnivore", "Trilobites not carnivore"),
  lwd = 2,
  lty = c(1, 2, 3),
  inset = c(0.05, 0.05),
  cex = 1.3
)




# total corrected
full_tsplot(
  x = df[df$diet == "Both", ]$mid,
  ys = list(df[df$diet == "Carnivore", ]$ext2f3, df[df$diet == "Non-Carnivore", ]$ext2f3),
  tit = "Extinctions",
  ylab = "Foote Metric",
  ylim = c(0, 2)
)

# relative extinction rates
full_tsplot(
  x = df[df$diet == "Both", ]$mid,
  ys = list(df[df$diet == "Carnivore", ]$extProp, df[df$diet == "Non-Carnivore", ]$extProp),
  tit = "Extinctions",
  ylab = "Proportion"
)
full_tsplot(
  x = df[df$diet == "Both", ]$mid,
  ys = list(carniAmmonExDiv$extProp, noncarniAmmonExDiv$extProp),
  tit = "Extinctions (ammonites counted as non-carnivores)",
  ylab = "Proportion"
)
full_tsplot(
  x = df[df$diet == "Both", ]$mid,
  ys = list(df_triloEx[df_triloEx$diet == "Carnivore", ]$extProp, nondf_triloEx[df_triloEx$diet == "Carnivore", ]$extProp),
  tit = "Extinctions (ammonites counted as non-carnivores)",
  ylab = "Proportion"
)

# zscored extinction rates (raw data, foote metric, since biases don't matter for this analysis)
zscore = function(x) {
  return (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
carniExtZscore = zscore(df[df$diet == "Carnivore", ]$extPC)
nonCarniExtZscore = zscore(df[df$diet == "Non-Carnivore", ]$extPC)

# Compare Z-scored values
full_tsplot(
  df[df$diet == "Both", ]$mid,
  ys = list(carniExtZscore, nonCarniExtZscore),
  ylim = c(-0.5, 1.5),
  tit = "Extinction Rates",
  ylab = "Foote Metric (Z-Scored)"
)

# difference between zscores
carniExtDiffZscore = carniExtZscore - nonCarniExtZscore
full_tsplot(
  df[df$diet == "Both", ]$mid,
  ys = list(carniExtDiffZscore),
  ylim = c(-0.5, 1.5),
  tit = "Difference between Carnivore Extinctions and others",
  ylab = "Foote Metric (Z-Scored)",
  cols = c("brown1"),
  labels = c("Carnivores - Non-Carnivores")
)


###### Analyzing extinction rates ########

library(ggplot2)
library(ggsignif)


comparisons = list(c("Carnivore", "Non-Carnivore"))

carniExt_value = df[df$mass_extinction == TRUE & df$diet == "Carnivore", ]$extPC
nonCarniExt_value = df[df$mass_extinction == TRUE & df$diet == "Non-Carnivore", ]$extPC
ext_pvalue = wilcox.test(carniExt_value, nonCarniExt_value, alternative =
                           "two.sided")[3]$p.value

carniNoExt_value = df[df$mass_extinction == FALSE & df$diet == "Carnivore", ]$extPC
nonCarniNoExt_value = df[df$mass_extinction == FALSE & df$diet == "Non-Carnivore", ]$extPC

non_ext_pvalue = wilcox.test(carniNoExt_value, nonCarniNoExt_value, alternative =
                               "two.sided")[3]$p.value


ggplot(df, aes(x = diet, y = extPC, colour = mass_extinction)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +
  geom_jitter(aes(color = mass_extinction, stroke = 1),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  geom_signif(
    y_position = 1.5,
    step_increase = 0.1,
    xmin = 0.8,
    xmax = 1.8,
    annotation = paste0("*** (Wilcox, p=", signif(non_ext_pvalue, digits = 3) , ")")
  ) +
  geom_signif(
    y_position = 1.35,
    step_increase = 0.1,
    xmin = 1.2,
    xmax = 2.2,
    annotation = paste0("NS (Wilcox, p=", signif(ext_pvalue, digits = 3), ")")
  ) +
  scale_color_manual(values = c("black", "red")) +
  theme_minimal() +
  labs(
    title = "Extinction Rates of Carnivores and Non-Carnivores",
    x = "Diet",
    y = "Second-for-Third Corrected Extinction Rate",
    color = "Mass Extinction"
  )


##### Permutation testing #######

n_iter = 1000
p_values <- numeric(n_iter)
n_min = 5 # number of mass extinctions

for (i in 1:n_iter) {
  carni_sub = df[df$mass_extinction == FALSE & df$diet == "Carnivore", ]
  noncarni_sub = df[df$mass_extinction == FALSE & df$diet == "Non-Carnivore", ]
  carni_sub <- carni_sub[sample(nrow(carni_sub), n_min), ]
  noncarni_sub <- noncarni_sub[sample(nrow(noncarni_sub), n_min), ]
  df_sub = rbind(carni_sub, noncarni_sub)

  test_result <- wilcox.test(extPC ~ diet, data = df_sub, alternative="two.sided")
  p_values[i] <- test_result$p.value
}

# Summary of results
summary(p_values)
hist(p_values, main = "P-value distribution from subsampling")
length(p_values[p_values < 0.05])
p_lower_bound = quantile(p_values, probs=c(0.025))
p_upper_bound = quantile(p_values, probs=c(0.975))

ggplot(df, aes(x = diet, y = extPC, colour = mass_extinction)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +
  geom_jitter(aes(color = mass_extinction, stroke = 1),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  geom_signif(
    map_signif_level = TRUE,
    y_position = 1.5,
    step_increase = 0.1,
    xmin = 0.8,
    xmax = 1.8,
    annotation = paste0("NS (Wilcox,", signif(p_lower_bound, digits = 3) ,"=<p<=", signif(p_upper_bound, digits = 3) , ", 95% CI)")
  ) +
  geom_signif(
    y_position = 1.35,
    step_increase = 0.1,
    xmin = 1.2,
    xmax = 2.2,
    annotation = paste0("NS (Wilcox, p=", signif(ext_pvalue, digits = 3), ")")
  ) +
  scale_color_manual(values = c("black","red")) +
  theme_minimal() +
  labs(
    title = "Extinction rates of Carnivores and Non-Carnivores, Subsampled",
    x = "Diet",
    y = "Second-for-Third Corrected Extinction Rate",
    color = "Mass Extinction"
  )


#### food shortage #####
food_ext_pvalue = wilcox.test(df[df$diet == "Carnivore" & df$food_mass_extinction == TRUE, ]$extPC, df[df$diet == "Non-Carnivore" & df$food_mass_extinction == TRUE, ]$extPC, alternative = "two.sided")[3]$p.value


ggplot(df, aes(x = diet, y = extPC, colour = food_mass_extinction)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +
  geom_jitter(aes(color = food_mass_extinction, stroke = 1),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  # geom_signif(
  #   map_signif_level = TRUE,
  #   y_position = 1.5,
  #   step_increase = 0.1,
  #   xmin = 0.8,
  #   xmax = 1.8,
  #   annotation = paste0("NS (Wilcox,", signif(p_lower_bound, digits = 3) ,"=<p<=", signif(p_upper_bound, digits = 3) , ", 95% CI)")
  # ) +
  geom_signif(
    y_position = 1.4,
    step_increase = 0.1,
    xmin = 1.2,
    xmax = 2.2,
    annotation = paste0("NS (Wilcox, p=", signif(food_ext_pvalue, digits = 3), ")")
  ) +
  scale_color_manual(values = c("black","red")) +
  theme_minimal() +
  labs(
    title = "Extinction rates of Carnivores and Non-Carnivores, Subsampled",
    x = "Diet",
    y = "Raw per Capita Extinction Rate",
    color = "Food Mass Extinction"
  )

######## rate splitting #########
# use ratesplit method to determine significant difference between carnivore and non-carnivore extinction rate across all bins
carnivores$group = "Carnivore"
nonCarnivores$group = "Non-Carnivore"

grouped_df = rbind(carnivores, nonCarnivores)
rs = ratesplit(grouped_df, sel="group", tax="genus", bin="stg")
rs

# extinction rate plot
tsplot(stages, boxes="sys", shading="series", xlim=4:95, ylim=c(0, 1),
       ylab="two-for-three corrected extinctions")
abline(v = c(65, 200, 250, 360, 444), col = "black", lty=2, lwd=2)

lines(stages$mid, df[df$diet == "Carnivore", ]$extPC, lwd=2, col="red")
lines(stages$mid, df[df$diet == "Non-Carnivore", ]$extPC, lwd=2, col="blue")
legend("topright", inset=c(0.1,0.1), legend=c("Carnivore", "Non-Carnivore"), 
       lwd=2, col=c("red", "blue"), bg="white")

# display selectivity with points
# select the higher rates
selIntervals<-cbind(df[df$diet == "Carnivore", ]$extPC[rs$ext], df[df$diet == "Non-Carnivore", ]$extPC[rs$ext])
groupSelector<-apply(selIntervals, 1, function(w) w[1]>w[2])
# draw the points
points(stages$mid[rs$ext[groupSelector]], df[df$diet == "Carnivore", ]$extPC[rs$ext[groupSelector]],
       pch=16, col="red", cex=2)
points(stages$mid[rs$ext[!groupSelector]], df[df$diet == "Non-Carnivore", ]$extPC[rs$ext[!groupSelector]],
       pch=16, col="blue", cex=2)


setwd('/home/leo/analytical_palaeobiology_project/')
df = read.csv('mass_extinction_divDyn.csv')
df_cephaEx = read.csv('mass_extinction_divDyn_cephalopoda_not_predators.csv')
df_triloEx = read.csv('mass_extinction_divDyn_trilobites_not_predators.csv')

carnivores = read.csv("carnivores.csv")
nonCarnivores = read.csv("non-carnivores.csv")
carnivores$group = "carnivore"
nonCarnivores$group = "non-carnivore"
grouped_df = rbind(carnivores, nonCarnivores)

##### display carnivore types #####

library(ggplot2)

carniPhyla = table(carnivores$phylum)
carniPhyla = sort(carniPhyla)
carniPhyla = as.data.frame(carniPhyla)

p = ggplot(data=carniPhyla, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#D1CCF0") +
  scale_y_log10() +
  geom_text(
    aes(label = Freq), 
    hjust = 1, nudge_y = -.1
  ) +
  
  coord_flip() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(
    title = "Phyla of Carnivores",
    x = "Phylum",
    y = "Fossil Count"
  )
p
ggsave("carnivores_phlyum_bar.png", plot = p, bg = "transparent", units="px", width = 1100, height = 900)


carniClass = table(carnivores$class)
carniClass = sort(carniClass)
carniClass = as.data.frame(carniClass)

p = ggplot(data=carniClass, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#D1CCF0") +
  scale_y_log10() +
  geom_text(
    aes(label = Freq), 
    hjust = 1, nudge_y = -.1
  ) +
  
  coord_flip() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(
    title = "Classes of Carnivores",
    x = "Class",
    y = "Fossil Count"
  )
p
ggsave("carnivores_class_bar.png", plot = p, bg = "transparent", units="px", width = 1500, height = 1200)


######## Time series diversitFreq######## Time series diversity & extinction ####

carniCol = "brown1"
nonCarniCol = "black"

library(divDyn)
data(stages)

full_tsplot <- function(x = NULL,
                        ys = NULL,
                        cols = c(carniCol, nonCarniCol),
                        labels = c("Carnivore", "Non-Carnivore"),
                        xlim = 4:95,
                        ylim = c(0, 1),
                        ylab = "Diversity Proportion",
                        boxes = "sys",
                        shading = "sys",
                        event_lines = c(65, 200, 250, 360, 444),
                        leg = TRUE,
                        legend_pos = "topright",
                        legend_inset = c(0.05, 0.05),
                        tit = "Diversity",
                        lgnd_title = "Diet") {
  # Plot the tsplot background
  tsplot(
    stages,
    boxes = boxes,
    shading = shading,
    xlim = xlim,
    ylim = ylim,
    ylab = ylab,
    shading.col = c(NA, "#D1CCF0"),plot.args = list(bg=NA)
  )
  
  # Add the lines
  for (i in seq_along(ys)) {
    lines(x, ys[[i]], col = cols[i], lwd = 2)
  }
  
  # Add vertical event lines
  abline(v = event_lines,
         col = "gray20",
         lty = 2, lwd=2)
  
  # add horizontal line
  abline(h = 0)
  
  
  # Add legend
  if (leg) {
    legend(
      legend_pos,
      bg = "white",
      legend = labels,
      title = lgnd_title,
      col = cols,
      lwd = 2,
      inset = legend_inset
    )
  }
  
  # Add title
  title(tit)
}


###### diversity rates #####
save_plot = function(filename,plot_wdth = 600, plot_height = 475, cex = 4) {
  dev.copy(png, filename, width = plot_wdth, height= plot_height)
  dev.off()
}
par(cex = 1.4, bg=NA)
full_tsplot(
  x = df[df$diet == "Both", ]$mid,
  ys = list(df[df$diet == 'Both', ]$divSIB),
  tit = "Diversity",
  ylab = "SIB Diversity",
  ylim = c(0, max(
    df[df$diet == "Both", ]$divCSIB, na.rm = TRUE
  ) * 1.1),cols = c("black"), leg =FALSE
)
save_plot("overallDiv.png")


full_tsplot(
  x = df[df$diet == "Both", ]$mid,
  ys = list(df[df$diet == 'Carnivore', ]$divSIB, df[df$diet == 'Non-Carnivore', ]$divSIB),
  tit = "Diversity",
  ylab = "SIB Diversity",
  ylim = c(0, max(
    df[df$diet == "Carnivore", ]$divSIB, df[df$diet == "Non-Carnivore", ]$divSIB, na.rm = TRUE
  ) * 1.1)
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
  )
  
}
total = df[df$diet=="Both",]$divSIB
carni = df[df$diet=="Carnivore",]$divSIB
proportion = carni / total

prop_tsplot(proportion, tit = "Porportion of Diversity by Diet")
save_plot("propDiv.png")


# exclude trilobites
total = df[df$diet == "Both", ]$divSIB
carni = df_triloEx[df_triloEx$diet == "Carnivore", ]$divSIB
proportion = carni / total
prop_tsplot(proportion, tit = "Proportion of Diversity by Diet (Trilobites Non-Carnivore)")
save_plot("propDivTrilo.png")

# exclude ammonites
total = df[df$diet == "Both", ]$divSIB
carni = df_cephaEx[df_cephaEx$diet == "Carnivore", ]$divSIB
proportion = carni / total
prop_tsplot(proportion, tit = "Proportion of Diversity by Diet (Cephalopods Non-Carnivore)")
save_plot("propDivCeph.png")


##### total extinction rates
full_tsplot(
  x = df[df$diet == "Both", ]$mid,
  ys = list(df[df$diet == "Carnivore", ]$extPC, df[df$diet == "Non-Carnivore", ]$extPC),
  tit = "Extinctions",
  ylab = "Per Capita Extinction Rate",
  ylim = c(0, 1.2)
)
save_plot("extinction.png", plot_wdth = 1000)


######## rate splitting #########
# use ratesplit method to determine significant difference between carnivore and non-carnivore extinction rate across all bins

rs = ratesplit(grouped_df, sel="group", tax="genus", bin="stg")
rs

# extinction rate plot
carnivore_extPC = df[df$diet == "Carnivore", ]$extPC
nonCarnivore_extPC = df[df$diet == "Non-Carnivore", ]$extPC
full_tsplot(stages$mid, ys = list(carnivore_extPC, nonCarnivore_extPC), ylim = c(0, 1.2), ylab = "Per Capita Extinction Rate", tit = "Rate Split Extinctions")


# display selectivity with points
# select the higher rates
selIntervals<-cbind(df[df$diet == "Carnivore", ]$extPC[rs$ext], df[df$diet == "Non-Carnivore", ]$extPC[rs$ext])
groupSelector<-apply(selIntervals, 1, function(w) w[1]>w[2])
# draw the points
points(stages$mid[rs$ext[groupSelector]], carnivore_extPC[rs$ext[groupSelector]],
       pch=16, col="red", cex=2)
points(stages$mid[rs$ext[!groupSelector]], nonCarnivore_extPC[rs$ext[!groupSelector]],
       pch=16, col="blue", cex=2)

save_plot("extinctions_highlight.png", plot_wdth = 1000)




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
median_diff = median(df[df$diet == "Carnivore",]$extPC, na.rm=TRUE) - median(df[df$diet == "Non-Carnivore",]$extPC, na.rm = TRUE)


p = ggplot(df[df$diet != "Both",], aes(x = diet, y = extPC, colour = mass_extinction)) +
  geom_boxplot(position = position_dodge(0.8), outlier.shape = NA) +
  geom_jitter(aes(color = mass_extinction, stroke = 1),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  geom_signif(
    y_position = 1.55,
    step_increase = 0.1,
    xmin = 0.8,
    xmax = 1.8,
    annotation = paste0(signif(median_diff, digits = 2),"*** (Wilcox, p=", signif(non_ext_pvalue, digits = 2) , ")")
  ) +
  geom_signif(
    y_position = 1.4,
    step_increase = 0.1,
    xmin = 1.2,
    xmax = 2.2,
    annotation = paste0("NS (Wilcox, p=", signif(ext_pvalue, digits = 2), ")")
  ) +
  scale_color_manual(values = c("black", "red")) +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),  # Transparent legend background
        legend.box.background = element_rect(fill = "transparent"))  +
  labs(
    title = "Extinction Rates in and out of Mass Extinctions",
    x = "Diet",
    y = "Per Capita Extinction Rate",
    color = "Mass Extinction"
  )
p
ggsave("mass_extinction_box.png", plot = p, bg = "transparent")


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


ggplot(df[df$diet != "Both", ], aes(x = diet, y = extPC, colour = food_mass_extinction)) +
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

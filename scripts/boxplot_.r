source('/Users/neekahaack/Desktop/Typas_internship/Michael-Knopp/scripts/utils.r')

#load in data
data <- read_csv2(
  here("data", "Main_screen.csv")
)
print(data)

#manipulate data
filtered_data <- data %>%
  #mutate(
  #  GFP = recode(GFP, "RFP" = "wt"),
  #  RFP = recode(RFP, "RFP" = "wt"),
  #) %>%
  filter(!(GFP == "wt" & RFP == "wt")) %>%
  mutate(
    Mutant = if_else(
      GFP == "wt",
      RFP,
      GFP),
    Condition = fct_inorder(Condition),
    Mutant = fct_inorder(Mutant),
    Mutant = recode(Mutant,
    "ompK35" = "ompK*",
    "OXA48" = "ompK*/Oxa48",
    "KPC2" = "ompK*/KPC2")
  ) %>%
  select(
    Mutant,
    Condition,
    selection_coefficient
  )
print(filtered_data, n=200)

#plot
plot_object <- ggplot(
  filtered_data,
  aes(
    x = Condition,
    y = selection_coefficient,
    fill = Mutant
  )
) +
scale_x_discrete(expand = expansion(add = 0.4)
) +
geom_hline(
  yintercept = 0,
  alpha = 0.3
) +
geom_boxplot(
  width = 0.5, 
  position = position_dodge(width = 0.6), #0.9
  outlier.shape = NA   # suppress outliers from boxplot
) +
geom_jitter(
  position = position_jitterdodge(dodge.width = 0.6, jitter.width = 0.1),
  alpha = 1,
  size = 0.1
) + # plot all points (including "outliers")
labs(
  x = NULL,
  y = "Selection Coefficient",
  fill = "Strain"
  #title = "Fitness Assay of Mutants"
) +
theme_presentation(
) +
scale_fill_brewer(palette = "Greys", name = "Strain"
) +
theme(
  axis.text.x = element_text(
    angle = 45,
    hjust = 1),
  axis.text.y = element_text(
    size = 8),
  legend.position = c(0.99, 0.97),
  legend.justification = c(1, 1),
  legend.text  = element_text(size = 8),
  legend.title = element_text(size = 9),
  legend.key.height = unit(10, "pt"),
  legend.key.width  = unit(10, "pt"),
  legend.background = element_rect(
    color = "black",
    linewidth = 0.4)
)

print(plot_object)

ggsave(
  plot=plot_object,
  filename = here(
    "results",
    "plots",
    "Fitness_assay_of_mutants_redo.pdf"
  ),
  width = 18,
  height = 5
)

# install.packages(c('tidyverse', 'patchwork'))
library(tidyverse)
library(patchwork)
d <- read_csv('F:/Manuscript_Year_2026/Bangladesh_UTCI/bangladesh_heat_stress.csv', show_col_types = FALSE) |>
  mutate(Division = factor(Division, levels = rev(sort(unique(Division)))),
         Year = factor(Year), Population_M = Total_Population / 1e6)
stopifnot(nrow(d) == 16, !anyDuplicated(d[c('Division','Year')]))
year_colors <- c('2000' = '#4382B4', '2020' = '#D45B36')
base <- theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank(), legend.position = 'bottom',
  plot.title = element_text(face = 'bold'), axis.title.y = element_blank())
dumbbell <- function(variable, title, xlabel, limits, breaks) {
  ggplot(d, aes(x = .data[[variable]], y = Division)) +
    geom_line(aes(group = Division), orientation = 'y', colour = 'grey65', linewidth = 0.8) +
    geom_point(aes(colour = Year, shape = Year), size = 2.8) +
    scale_colour_manual(values = year_colors) +
    scale_shape_manual(values = c('2000' = 16, '2020' = 17)) +
    scale_x_continuous(limits = limits, breaks = breaks) +
    labs(title = title, x = xlabel, colour = NULL, shape = NULL) + base
}
p1 <- dumbbell('Population_M', '(a) Total population', 'Population (million)', c(0,40), seq(0,40,10))
p2 <- dumbbell('Moderate_Stress_Pct', '(b) Moderate stress', 'Population exposed (%)', c(0,100), seq(0,100,25))
p3 <- dumbbell('Strong_Stress_Pct', '(c) Strong stress', 'Population exposed (%)', c(0,40), seq(0,40,10))
no_y <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
fig1 <- (p1 | (p2 + no_y) | (p3 + no_y)) + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
print(fig1)
ggsave('01_dumbbell.png', fig1, width = 10, height = 4.5, dpi = 600)
ggsave('01_dumbbell.pdf', fig1, width = 10, height = 4.5)

# Option 2
stopifnot(all(d$Moderate_Stress_Pct + d$Strong_Stress_Pct <= 100))
exposure <- d |> mutate(Moderate = Population_M * Moderate_Stress_Pct / 100,
 Strong = Population_M * Strong_Stress_Pct / 100,
 Other = Population_M - Moderate - Strong) |>
 pivot_longer(c(Other, Moderate, Strong), names_to = 'Stress', values_to = 'Population') |>
 mutate(Stress = factor(Stress, levels = c('Other','Moderate','Strong')))
fig2 <- ggplot(exposure, aes(Population, Year, fill = Stress)) +
 geom_col(position = position_stack(reverse = TRUE), width = .7) +
 facet_grid(Division ~ ., switch = 'y') +
 scale_fill_manual(values = c(Other = '#BFC9C4', Moderate = '#E9AD43', Strong = '#B83B32')) +
 labs(x = 'Population (million)', y = NULL, fill = NULL) + base +
 theme(strip.background = element_blank(), strip.placement = 'outside', strip.text.y.left = element_text(angle = 0), panel.spacing.y = unit(0.15,'lines'))
print(fig2)
ggsave('02_stacked.png', fig2, width = 8, height = 7, dpi = 600)
ggsave('02_stacked.pdf', fig2, width = 8, height = 7)


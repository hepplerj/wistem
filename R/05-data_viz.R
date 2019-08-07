# Data visualizations
library(jahMisc)
rotate_x_axis <- function() {
  ggplot2::theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))
}

theme_agile <- function(base_size = 10, base_family = "Arial", lines_lwd = 0.50, plot_grid = TRUE, axis_font = base_family, title_size = base_size*1.1, legend_size = base_size,
                        bg_col = "white",title_font = base_family , base_col  = "black", axis_lines = TRUE,
                        minor_grid = ifelse(plot_grid, TRUE, FALSE), vert_grid = ifelse(plot_grid, TRUE, FALSE), ticks_type = "outer", horz_grid = ifelse(plot_grid, TRUE, FALSE), alpha_leg = 0.1, bord_size = 0,
                        legend_bg = "white", strip_bg = "white", grid_thick = 1,
                        grid_type = "solid", ticks_xy  = "xy", grid_cols = c("grey50", "grey70")){
  theme_bw()+
    ggplot2::theme(
      plot.margin = grid::unit(c(1, 1, .5, .7), "cm"),
      text = ggplot2::element_text(family = base_family, size = base_size),
      axis.line =  element_line(size = ifelse(axis_lines, grid::unit(lines_lwd, "mm"),0), color = "black"),
      axis.ticks.length = grid::unit(ifelse(ticks_type == "outer", 0.15, -0.15), "cm"),
      axis.ticks.x =  element_line(size = ifelse(stringr::str_detect(ticks_xy, "x"), grid::unit(lines_lwd, "cm"),0), color = "black"),
      axis.ticks.y =  element_line(size = ifelse(stringr::str_detect(ticks_xy, "y"), grid::unit(lines_lwd, "cm") ,0), color = "black"),
      axis.text.x = ggplot2::element_text(size = base_size, colour = base_col , family = axis_font,margin=margin(ifelse(ticks_type == "inner", 11, 5),5,10,5,"pt")),
      axis.text.y = ggplot2::element_text(size = base_size, colour = base_col , family = axis_font, margin=margin(5,ifelse(ticks_type == "inner", 11, 5),10,5,"pt")),
      axis.title.y = ggplot2::element_text(size =  base_size, colour = base_col , vjust = 1.5, family = axis_font),
      axis.title.x = ggplot2::element_text(size = base_size,colour = base_col ,vjust = -.5, family = axis_font),
      panel.background = ggplot2::element_rect(fill = bg_col),
      plot.background = ggplot2::element_rect(fill = bg_col),
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size = bord_size),
      panel.grid.major.x = ggplot2::element_line(linetype = grid_type,colour = ifelse(vert_grid, grid_cols[1],bg_col), size = ifelse(vert_grid,0.25 * grid_thick, 0)),
      panel.grid.minor.x = ggplot2::element_line(linetype = grid_type,colour = ifelse(vert_grid, ifelse(minor_grid, grid_cols[2 - (length(grid_cols) == 1)   ],bg_col),bg_col), size = ifelse(vert_grid,0.15* grid_thick, 0)),
      panel.grid.major.y = ggplot2::element_line(linetype = grid_type,colour = ifelse(horz_grid, grid_cols[1],bg_col), size = ifelse(horz_grid,0.25* grid_thick, 0)),
      panel.grid.minor.y = ggplot2::element_line(linetype = grid_type,colour = ifelse(horz_grid, ifelse(minor_grid, grid_cols[2 - (length(grid_cols) == 1)  ],bg_col),bg_col), size = ifelse(horz_grid,0.15* grid_thick, 0)),
      plot.title = ggplot2::element_text(face="bold",vjust = 2, colour = base_col , size = title_size, family = title_font),
      legend.background = ggplot2::element_rect(fill = scales::alpha(legend_bg, alpha_leg)), legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = legend_size, family = base_family),
      legend.title = element_blank(),
      strip.background =  ggplot2::element_rect(colour = strip_bg, fill = strip_bg),
      strip.text.x = ggplot2::element_text(size = base_size + 1),
      strip.text.y = ggplot2::element_text(size = base_size + 1)
    )
}

# Plot the number of publications per year
ggplot(data %>% drop_na(date), aes(x = date)) +
  geom_bar() +
  theme_minimal()

# Plot the number of articles per publication
ggplot(data, aes(x = pubtitle)) +
  geom_bar() +
  theme_minimal()

# Scatterplot of name and position
# Not a particularly useful view into the data.
ggplot(author_order, aes(x = author, y = position)) +
  geom_point(aes(color=gender))

# Faceted barchart of gender by publication
# Not a particularly useful view into the data.
ggplot(author_order, aes(position, fill = gender)) +
  geom_bar() +
  facet_grid(pubtitle ~ .)

# Authorship by position
# Useful chart.
ggplot(author_order, aes(position)) +
  geom_bar() +
  facet_grid(. ~ gender) +
  labs(title = "Authorship position by gender") +
  theme_jah()

# Authorship by year
first_authorship_by_year <- ggplot(author_order %>% filter(position == 1) %>% drop_na(date, gender), aes(date)) +
  geom_bar() +
  facet_grid(. ~ gender) +
  labs(title = "First author gender by year",
       caption = "CC-BY | Heidi Blackburn and Jason Heppler") +
  fte_theme() +
  rotate_x_axis() +
  theme(panel.background=element_rect(fill="white", color="white")) +
  theme(plot.background=element_rect(fill="white", color="white")) +
  theme(panel.border=element_rect(color="white"))
ggsave(filename = "analysis/figures/first_authorship_by_year.jpg", plot = first_authorship_by_year, dpi = 300)
ggsave(filename = "analysis/figures/first_authorship_by_year.tiff", plot = first_authorship_by_year, dpi = 300)

# Stacked barchart of gender by year
# Useful chart.
ggplot(author_order, aes(x = date, fill = gender)) +
  geom_bar() +
  theme_minimal()

# Gender by journal vs. conference
# Useful chart.
ggplot(conf_journal_merged, aes(x = date, fill = gender)) +
  geom_bar() +
  facet_grid(~ pubtype) +
  theme_light()

# Counting the number of authors by position
author_order %>%
  count(position) %>%
  ggplot(aes(x = position, y = n)) + geom_bar(stat = "identity") +
  theme_minimal()

# Graph: Top ten journals that saw growth
journal_names <- c(
  `Journal of Diversity in Higher Education` = "Divers High Educ",
  `Frontiers in Psychology` = "Front Psychol",
  `Journal of Science Education and Technology` = "Sci Educ Technol",
  `Journal of vocational behavior` = "Vocat Behav",
  `Journal of Women and Minorities in Science and Engineering` = "Women Minor Sci Eng",
  `PLoS ONE` = "PLoS",
  `Psychology of Women Quarterly` = "Psychol Women Q",
  `Research in Higher Education` = "Res High Educ",
  `Sex Roles` = "Sex Roles",
  `Social Sciences` = "Soc Sci"
)

ggplot(journals_top_ten_abbr) +
  geom_bar(aes(x = date)) +
  facet_grid(rows = vars(pubtitle), 
             labeller = labeller(pubtitle = journal_names)) + #, labeller = as_labeller(journal_names)) +
  theme_agile() +
  theme(strip.text.y = element_text(size = 6),
        strip.placement = "outside")

publisher_names <- c(
  `American Psychological Association` = "APA",
  `American Society of Engineering Education` = "ASEE",
  `Begell House, Inc.` = "Begell",
  `Elsevier Inc.` = "Elsevier",
  `Frontiers Research Foundation` = "Frontiers",
  `M D P I AG` = "MDPIAG",
  `Routledge` = "Routledge",
  `Sage Publications, Inc.` = "Sage",
  `Springer New York LLC` = "Springer",
  `Wiley-Blackwell Publishing, Inc.` = "Wiley"
)

ggplot(publishers_top_ten_abbr) +
  geom_bar(aes(x = date)) +
  facet_grid(rows = vars(publisher),
             labeller = labeller(publisher = publisher_names)) +
  theme_agile() +
  theme(strip.text.y = element_text(size = 10))

conference_names <- c(
  `2014 IEEE Frontiers in Education Conference (FIE) Proceedings` = "2014 FIE",
  `7th IEEE GCC Conference and Exhibition (GCC)` = "GCC",
  `ACM Conference on Innovation and Technology in Computer Science Education` = "ACM Innov. Tech.",
  `ACM International Conference on Measurement and Modeling of Computer Systems` = "ACM Meas. Mod.",
  `AIP Conference Proceedings` = "AIP",
  `ASEE Annual Conference and Exposition, Conference Proceedings` = "ASEE",
  `Proceedings of the 7th IEEE Integrated STEM Education Conference` = "7th STEM Ed.",
  `2007 37th Annual Frontiers In Education Conference - Global Engineering: Knowledge Without Borders, Opportunities Without Passports` = "2007 Frontiers"
)
#conference_top_ten_abbr <- conference_top_ten_abbr %>% filter(pubtitle != "Proceedings of the National Academy of Sciences of the United States of America")

ggplot(conference_top_ten_abbr %>% drop_na(date)) +
  geom_bar(aes(x = date)) +
  facet_grid(rows = vars(pubtitle),
             labeller = labeller(pubtitle = conference_names)) +
  theme_agile() +
  theme(strip.text.y = element_text(size = 8))

# Graph: Journals that support women as first author 
# top 25 journals with female first authors
ggplot(female_journalfirst_author %>% filter(n > 5), aes(reorder(pubtitle, -n))) + 
  geom_bar() +
  theme_minimal() +
  theme_agile() +
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5)) 


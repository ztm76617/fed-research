#-----------------------------------------------------------
# Custom ggplot theme FACET
#-----------------------------------------------------------
ggplot_theme_FACET <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(text = element_text(face = 'bold'),
          axis.text.x = element_text(size = rel(1), angle = 35),
          panel.background = element_rect(colour = "black"),
          strip.text.x = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, size = rel(0.9)),
          plot.caption.position = "plot",
          legend.position = "bottom",
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")
    )
}
#-----------------------------------------------------------
# Custom ggplot theme NO FACET
#-----------------------------------------------------------
ggplot_theme_NON_FACET <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(text = element_text(face = 'bold'),
          axis.text.x = element_text(size = rel(1)),
          panel.background = element_rect(colour = "black"),
          strip.text.x = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, size = rel(0.9)),
          plot.caption.position = "plot",
          legend.position = "bottom",
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")
    )
}
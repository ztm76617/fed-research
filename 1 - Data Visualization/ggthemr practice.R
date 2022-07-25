#--------------------------------------------------------------------------------
# ggthemr themes
#1 fresh
#2 grape
#3 greyscale
#4 light
#5 sky
#6 solarized
#7 dust

ggthemr('fresh')
swatch()
ggthemr_reset()
#--------------------------------------------------------------------------------
ggthemr_reset()
custom_theme <- ggthemr('dust', set_theme = FALSE)
ggthemr('dust_theme')


tableau_colours <- c('#59595c', '#111111', '#65ADC2', '#b2432f', '#9467BD', '#109B37', '#9b5672', '#908150', '#373634', '#c43d31')
ggthemr_reset()
tableau <- define_palette(
  swatch = tableau_colours, 
  gradient = c(lower = tableau_colours[1L], upper = tableau_colours[2L]))
ggthemr(tableau)
#--------------------------------------------------------------------------------
ggthemr('fresh')
swatch()

ggplot(mpg, aes(class)) +
  geom_bar(aes(fill = drv)) +
  theme_bw() +
  theme(text = element_text(face = 'bold', size = 9),
        axis.title.x = element_text(vjust = -0.75),
        axis.title.y = element_text(vjust = 0.75),
        panel.background = element_rect(colour = "black"),
        strip.text.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

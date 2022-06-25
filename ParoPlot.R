pacman::p_load(tidyverse,ggrepel,ggtext,showtext, rcartocolor, patchwork)
'%!in%' <- function(x,y)!('%in%'(x,y))

load("/Users/Pito/Downloads/Paro.RData")

font_add_google("Lato")
#showtext_auto()

####################################################
#Prepare data

data1 <- data %>% 
	# Extract year
	mutate(year = lubridate::year(anno)) %>% 
	# Subset variables
	select(anno, year, cod_prov, prov, paro_tot) %>% 
	# If there is more than one record per year/country, use the mean 
	group_by(cod_prov, prov, year) %>% 
	summarize(avgparoyear = mean(paro_tot)) %>% 
	group_by(cod_prov)

#####################################################
# Also define the group of countries that are going to be highlighted
highlights <- c("Zaragoza", "Sevilla", "Santa Cruz de Tenerife", "Palmas, Las", "León", "Barcelona", "Madrid", "Ceuta")
n <- length(highlights)

######################################################
#Theme definition

# This theme extends the 'theme_minimal' that comes with ggplot2.
theme_set(theme_minimal(base_family = "Times"))
theme_update(
	# Remove title for both x and y axes
	axis.title = element_blank(),
	# Axes labels are grey
	axis.text = element_text(color = "grey40"),
	# The size of the axes labels are different for x and y.
	axis.text.x = element_text(size = 20, margin = margin(t = 5)),
	axis.text.y = element_text(size = 17, margin = margin(r = 5)),
	# Also, the ticks have a very light grey color
	axis.ticks = element_line(color = "grey91", size = .5),
	# The length of the axis ticks is increased.
	axis.ticks.length.x = unit(1.3, "lines"),
	axis.ticks.length.y = unit(.7, "lines"),
	# Remove the grid lines that come with ggplot2 plots by default
	panel.grid = element_blank(),
	# Customize margin values (top, right, bottom, left)
	plot.margin = margin(20, 40, 20, 40),
	# Use a light grey color for the background of both the plot and the panel
	plot.background = element_rect(fill = "grey98", color = "grey98"),
	panel.background = element_rect(fill = "grey98", color = "grey98"),
	# Customize title appearence
	plot.title = element_text(
		color = "grey10", 
		size = 28, 
		face = "bold",
		margin = margin(t = 15)
	),
	# Customize subtitle appearence
	plot.subtitle = element_markdown(
		color = "grey30", 
		size = 16,
		lineheight = 1.35,
		margin = margin(t = 15, b = 40)
	),
	# Title and caption are going to be aligned
	plot.title.position = "plot",
	plot.caption.position = "plot",
	plot.caption = element_text(
		color = "grey30", 
		size = 13,
		lineheight = 1.2, 
		hjust = 0,
		margin = margin(t = 40) # Large margin on the top of the caption.
	),
	# Remove legend
	legend.position = "none"
)
######################################################
#Plot

plt <- ggplot(
	# The ggplot object has associated the data for the highlighted countries
	data1 %>% filter(prov %in% highlights), 
	aes(year, avgparoyear, group = prov)
) + 
	# Geometric annotations that play the role of grid lines
	geom_vline(
		xintercept = seq(2008, 2014, by = 1),
		color = "grey91", 
		size = .6
	) +
	geom_segment(
		data = tibble(y = seq(-4, 3, by = 1), x1 = 2008, x2 = 2014),
		aes(x = x1, xend = x2, y = y, yend = y),
		inherit.aes = FALSE,
		color = "grey91",
		size = .6
	) +
	geom_segment(
		data = tibble(y = 0, x1 = 2008, x2 = 2014),
		aes(x = x1, xend = x2, y = y, yend = y),
		inherit.aes = FALSE,
		color = "grey60",
		size = .8
	) +
	geom_vline(
		aes(xintercept = 2009), 
		color = "grey40",
		linetype = "dotted",
		size = .8
	) +
	## Lines for the non-highlighted countries
	geom_line(
		data = data1 %>% filter(prov %!in% highlights),
		color = "grey75",
		size = .6,
		alpha = .5
	) +
	## Lines for the highlighted countries.
	# It's important to put them after the grey lines
	# so the colored ones are on top
	geom_line(
		aes(color = prov),
		size = .9
	)

######################################################
#Add text annotations

#First we define the vector of labels
data1 <- data1 %>% add_column(name_lab = NA_character_) 

data1<- data1 %>%	mutate(
	name_lab = case_when(
		data1$prov[i] %in% highlights ~ data1$prov[i]))

for(i in seq_along(data1$name_lab)){
	if(data1$prov[i] %in% highlights){
		if(data1$year[i] == 2014){
		data1$name_lab[i] = data1$prov[i]
		}
	}
}

plt <- plt +
	geom_text_repel(
		aes(color = prov, 
				label = name_lab),
		family = "Times",
		fontface = "bold",
		size = 5.5,
		direction = "y",
		xlim = c(2015, NA),
		hjust = 0,
		segment.size = .5,
		segment.alpha = .5,
		segment.linetype = "dotted",
		box.padding = .4,
		segment.curvature = -0.1,
		segment.ncp = 3,
		segment.angle = 20
	) +
	# Able drawings outside plot area
	coord_cartesian(
		clip = "off",
	) +
	scale_x_continuous(
		expand = c(0, 0),
		limits = c(2008, 2015.5), 
		breaks = seq(2008, 2014, by = 1)
	) +
	scale_y_continuous(
		expand = c(0, 0),
	) 

plt <- plt + labs(
	title = "Paro en España por provincias",
	subtitle = "Paro total medio por año, a partir del 2008 cuando empezó la crisis.",
	caption = "Vis by Alessio Crisafulli Carpani  •  Datos del INE"
)
	
plt

ggsave("paroINE")
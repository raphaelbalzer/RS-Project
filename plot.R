library(readxl)
library(extrafont)
library(tidyverse)
library(ggthemes)
library(gganimate)
library(gifski)
data <- read_excel("C:/Users/rapha/OneDrive/Desktop/Kopie von API_EN.ATM.CO2E.KT_DS2_en_excel_v2_5455447.xls")
View(Kopie_von_API_EN_ATM_CO2E_KT_DS2_en_excel_v2_5455447)
# Select countries and relevant years
countries <- c("Saudi Arabia", "Germany", "Russian Federation", "India", "China", "United States")
years <- as.character(1990:2019)

# Filter and reshape the data
data_filtered <- data %>%
  filter(`Country Name` %in% countries) %>%
  select(`Country Name`, years) %>%
  pivot_longer(cols = -`Country Name`, names_to = "Year", values_to = "Emissions") %>%
  filter(Year %in% years) %>%
  mutate(Year = as.numeric(Year))

data_filtered[data_filtered=="Germany"] <- "Deutschland"
data_filtered[data_filtered=="India"] <- "Indien"
data_filtered[data_filtered=="Russian Federation"] <- "Russland"
data_filtered[data_filtered=="Saudi Arabia"] <- "Saudi Arabien"
data_filtered[data_filtered=="United States"] <- "USA"

data_filtered <- 
  data_filtered %>% 
  rename(Land = `Country Name`)

# Plot CO2 emissions for the selected countries
static <- ggplot(data_filtered, aes(x = Year, y = Emissions, color = Land)) +
  geom_path(linewidth = 1.4, alpha = 0.75, lineend = "square") + 
  scale_y_continuous(labels = function(x) paste0(x/1000000, "M")) +
  labs(x = "Jahr", 
       y = "CO2-Emissionen in kt", 
       title = "CO2-Emissionen im Zeitraum von 1990 - 2019") +
  theme_minimal() +
  theme(axis.title = element_text(), text = element_text(family = "Sitka Text")) + 
  scale_color_colorblind()


animated_graph <- static + transition_reveal(Year)

animated_graph_final <- animate(animated_graph,
        height = 500, width = 600,
        fps = 30, 
        duration = 10,
        end_pause = 30, 
        res = 150)
animated_graph_final
anim_save("animated_graph.gif")
static

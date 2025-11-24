install.packages(tidyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(dbscan)
library(patchwork)
library(ggmap)

# Fixer le répertoire de travail
setwd("/home/nicolas/Documents/03_data")

df_uk_mj <- read.csv("./uk_maniola_jurtina.csv", sep ="\t") %>%
  filter(decimalLatitude >= 30 | decimalLongitude >= 0) 
df_uk_pr <- read.csv("./uk_pieris_rapae.csv", sep ="\t")
nrow(df_uk_mj)
#### Carte du UK #### 
uk <- st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_GBR_0.json")

plot(uk["COUNTRY"])
#### Afficher les points d'observations####
points <- df_uk_mj %>%
  distinct(decimalLatitude, decimalLongitude) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) 

ggplot() +
  geom_sf(data = uk, fill = NA, color = "black") +
  geom_sf(data = points, color = "red") + 
  theme_minimal() 



#### Carte par année ####

unique_year <- df_uk_mj%>%
  pull(year) %>%
  unique() %>%
  sort()

print(unique_year)
plots_mj <- list()
plots_pr <- list()
nb_obs_mj <- list()
nb_obs_pr <- list()
for (i in seq_along(unique_year)){
  yr <- unique_year[i]
  df_mj_year <- df_uk_mj %>% filter(year == yr)
  nb_obs_mj[[i]] <- nrow(df_mj_year)
  points_year <- df_mj_year %>%
    #    distinct(decimalLatitude, decimalLongitude) %>%
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
    st_transform(crs = 2154)
  
  plot <- ggplot() +
    geom_sf(data = uk, fill = NA, color = "black") +
    geom_sf(data = points_year, color = "red", size = 0.1) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title = element_blank(),   
      axis.text = element_blank(),    
      axis.ticks = element_blank()) +
    labs(title = yr)
  
  plots_mj[[i]] <- plot
  
}
for (i in seq_along(unique_year)){
  yr <- unique_year[i]
  df_pr_year <- df_uk_pr %>% filter(year == yr)
  nb_obs_pr[[i]] <- nrow(df_pr_year)
  
  points_year <- df_pr_year %>%
    #    distinct(decimalLatitude, decimalLongitude) %>%
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
    st_transform(crs = 2154)
  
  plot <- ggplot() +
    geom_sf(data = uk, fill = NA, color = "black") +
    geom_sf(data = points_year, color = "red", size = 0.1) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),
      axis.title = element_blank(),   
      axis.text = element_blank(),    
      axis.ticks = element_blank()) +
    labs(title = yr)
  
  plots_pr[[i]] <- plot
  
}

nb_obs_mj <- unlist(nb_obs_mj)
nb_obs_pr <- unlist(nb_obs_pr)
str(nb_obs_mj)
df_obs <- data.frame(
  year = unique_year,
  Maniola_jurtina = nb_obs_mj,
  Pieris_rapae = nb_obs_pr)
head(df_obs)

df_long <- df_obs %>%
  pivot_longer(
    cols = c(Maniola_jurtina, Pieris_rapae),
    names_to = "species",
    values_to = "n_obs"
  )

ggplot(df_long, aes(x = year, y = n_obs, color = species)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Année",
    y = "Nombre d'observations",
    color = "Espèce"
  ) +
  theme_minimal()

plot(unique_year, nb_obs_pr, 
     type = "o", 
     col = "blue",
     pch = 4,
     xlab = "Année", 
     ylab = "Nombre d'observations") 

wrap_plots(plots, ncol = 5, nrow = 4)
wrap_plots(plots[seq(1, length(plots), by = 2)], ncol = 5, nrow = 2)

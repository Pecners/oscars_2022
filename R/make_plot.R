library(tidyverse)
library(ggstar)
library(showtext)
library(ggtext)
library(geomtextpath)
library(ggbeeswarm)
library(glue)

# Set movie code for plot

movie_code <- "tt7740496"

# Pull principal cast 

pd_actors <- principals %>%
  filter(category %in% c("actor", "actress") &
           tconst == movie_code) %>%
  left_join(., names)

# Create df with filmography of principal cast
# including movie names, ratings, known_for

all_movies <- principals %>%
  filter(category %in% c("actor", "actress") &
           nconst %in% pd_actors$nconst) %>%
  left_join(., names) %>%
  left_join(., ratings) %>%
  left_join(., movies %>%
              select(tconst, 
                     primaryTitle,
                     startYear))

# Clean up df 

clean <- all_movies %>%
  filter(!is.na(averageRating)) %>%
  mutate(is_known_for = str_detect(knownForTitles, tconst)) %>%
  select(tconst,
         primaryTitle,
         startYear,
         nconst,
         primaryName,
         characters,
         ordering,
         birthYear,
         averageRating,
         numVotes,
         is_known_for)

# Set branding colors

gold <- "#EBAB47"
red <- "#89221B"
blue <- "#191B7A"

font_add_google("Frank Ruhl Libre", "frl")
showtext_auto()
showtext_opts(dpi = 100)

# Get primary cast ordering

order <- clean %>%
  filter(tconst == movie_code) %>%
  select(primaryName, 
         ordering) %>%
  arrange(ordering)

clean <- clean %>%
  mutate(primaryName = factor(primaryName, levels = order$primaryName))

# Get avg movie rating per actor

actor_avg <- clean %>%
  group_by(primaryName) %>%
  summarise(avg = mean(averageRating))

# Set labels that have the actor head shots

labels <- c(
  glue("<img src='images/bradley_cooper.jpg' height='75' style='border-radius:5px' /><br>",
  "**Bradley Cooper**"),
  glue("<img src='images/cate_blanchett.jpg' height='75' style='border-radius:5px' /><br>",
  "**Cate Blanchett**"),
  glue("<img src='images/toni_collette.jpg' height='75' style='border-radius:5px' /><br>",
  "**Toni Collette**"),
  glue("<img src='images/willem_dafoe.jpg' height='75' style='border-radius:5px' /><br>",
  "**Willem Dafoe**")
)

# Extract this movie average for use in subtitle

pd_rating <- clean %>%
  filter(tconst == movie_code) %>%
  .[["averageRating"]] %>%
  unique()

# Dataframe for top and bottom rated movies for each 
# principal cast member

top_bottom <- clean %>%
  group_by(primaryName) %>%
  filter(averageRating == max(averageRating) |
           averageRating == min(averageRating)) %>%
  mutate(g = ifelse(averageRating == max(averageRating), "max", "min")) %>%
  ungroup() %>%
  group_by(primaryName, averageRating) %>%
  filter(numVotes == max(numVotes)) 

# Make the plot

pd_plot <- clean %>%
  filter(tconst != movie_code) %>%
  ggplot(aes(primaryName, averageRating)) +
  geom_violin(data = clean, color = red, fill = red, alpha = .1, 
              scale = "count", width = .5) +
  geom_quasirandom(alpha = .3, width = .15, size = 2.5,
                   shape = 22, fill = red, varwidth = TRUE)  +
  geom_segment(data = actor_avg, inherit.aes = FALSE,
                    aes(x = as.numeric(primaryName) - (.75 / 2), 
                        xend = as.numeric(primaryName) + (.75 / 2),
                        y = avg, yend = avg), color = red,
                    linetype = 2) +
  geom_star(data = clean %>%
              filter(tconst == movie_code), alpha = .9,
            fill = gold, size = 4) +
  geom_text(data =  top_bottom,
            aes(label = str_wrap(primaryTitle, 40),
                y = ifelse(g == "max",
                               averageRating + .4, 
                           averageRating - (.3 * ceiling(str_length(primaryTitle) / 80)))),
            color = red, family = "frl",
            lineheight = .85, size = 3) +
  scale_color_identity() +
  scale_x_discrete(labels = labels) +
  scale_y_continuous(limits = c(1, 10),
                     breaks = c(1, 5, 10), labels = c(1, 5, 10)) +
  theme_minimal() +
  theme(text = element_text(family = "frl"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_textbox(size = 26, hjust = .5, halign = .5,
                                  face = "bold", color = gold,
                                  fill = red, linetype = 1, linewidth = .75,
                                  lineheight = .25,
                                  padding = margin(5, 5, 0, 5), 
                                  margin = margin(t = 5, b = 5),
                                  r = unit(3, "pt")),
        plot.subtitle = element_textbox_simple(color = red, size = 14,
                                               margin = margin(b = 20, t = 10)),
        plot.caption = element_textbox(color = red, fill = alpha(gold, .25),
                                       linetype = 1, linewidth = .1,
                                       padding = margin(3, 3, 2, 3),
                                       width = unit(9.3, "in"),
                                       lineheight = 1.1),
        axis.text.x = element_markdown(size = 14, color = red),
        axis.text.y = element_text(color = red),
        axis.title.x = element_textbox(color = gold, size = 12,
                                        halign = .5, fill = red,
                                        padding = margin(2, 3, 2, 3),
                                        margin = margin(b = 3),
                                        linetype = 1,
                                        r = unit(3, "pt")),
        axis.title.y = element_textbox(color = gold, orientation = "left",
                                       halign = .5, fill = red, size = 12,
                                       padding = margin(2, 3, 2, 3),
                                       margin = margin(b = 3),
                                       linetype = 1,
                                       r = unit(3, "pt"))) +
  labs(title= glue("NIGHTMARE ALLEY<span style='font-size:12pt'><br>",
                   "Where does it rank among the cast's filmography?",
                   "</span>"),
       subtitle = glue(
         "**Nightmare Alley** received an Oscar nomination for Best Picture this year. ",
         "With an average IMDb rating of **{pd_rating}**, this film ranks above average ",
         "among the extensive filmography of the principal cast."
       ),
       y = "Average IMDb Rating",
       x = "Principal Cast",
       caption = glue(
         "Each square represents a film from the cast member's filmography, ",
         "and the star represents the movie named in the plot title. Highest and lowest ",
         "rated movies in the cast member's filmography labeled with text. ",
         "The dotted line represents the overall average IMDb rating for the cast member's filmography. ",
         "The outlines are violin plots, where wider areas represent ",
         "bands with higher proportion of ratings. ",
         "Analysis and graphic by Spencer Schien (@MrPecners), data sourced from IMDb ",
         "on March 9, 2022."
       ))


ggsave(pd_plot, filename = "plots/nightmare_alley.png", bg = "white",
       width = 9.5, height = 7.5)

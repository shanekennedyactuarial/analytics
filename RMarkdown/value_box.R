library(ggplot2)
#install.packages("emojifont")
library(emojifont)
df <- data.frame(
  x = rep(seq(2, 15, 6.5), 2),
  y = c(rep(6.5, 3), rep(2,3)),
  h = rep(4.25, 6),
  w = rep(6.25, 6),
  value = c("78%",
            "+10K",
            "8/10",
            "ALL",
            "6",
            "< 0.5"),
  info = c("meaningless plots",
           "hours wasted",
           "zombies prefer brains",
           "dogs go to heaven",
           "infoboxes",
           "target pvalue"),
  icon = c(fontawesome(search_fontawesome("chart")),
           emoji("athletic_shoe")),
  font_family = c(rep("fontawesome-webfont", 5),
                  "EmojiOne"),
  color = factor(1:6)
)

ggplot(df, aes(x, y, height = h, width = w, label = info)) +
  ## Create the tiles using the `color` column
  geom_tile(aes(fill = color)) +
  ## Add the numeric values as text in `value` column
  geom_text(color = "white", fontface = "bold", size = 10,
            aes(label = value, x = x - 2.9, y = y + 1), hjust = 0) +
  ## Add the labels for each box stored in the `info` column
  geom_text(color = "white", fontface = "bold",
            aes(label = info, x = x - 2.9, y = y - 1), hjust = 0) +
  coord_fixed() +
  scale_fill_brewer(type = "qual",palette = "Dark2") +
  ## Use `geom_text()` to add the icons by specifying the unicode symbol.
  geom_text(size = 20, aes(label = icon, family = font_family,
                           x = x + 1.5, y = y + 0.5), alpha = 0.25) +
  theme_void() +
  guides(fill = FALSE)

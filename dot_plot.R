library(ggpubr)


long <- melt(df, id.vars = c("independent_variable", "out_variable", "group"), measure.vars = c("out_delta"))

head(df)


df<-df[!(df$out_delta==0),]

ggdotchart(df, x = "out_variable", y = "out_delta",
           color = "independent_variable",                                # Color by groups
           palette = "Paired", # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE, position = position_dodge(0.3),                               # Rotate vertically
           dot.size = 2,                                 # Large dot size
           y.text.col = TRUE,                            # Color y text by groups
           ggtheme = theme_pubclean(), facet.by = "group"                        # ggplot2 theme
)+
  theme_cleveland()                              # Add dashed grids

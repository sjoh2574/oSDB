library(ggpubr)
library(dplyr)
adjust_per <- 151

data$group <- rep(c(1:(nrow(data)/adjust_per)), each = adjust_per)

data <- data %>%
  group_by(group) %>%
  mutate(out_p_adj = p.adjust(out_pprop, method = "fdr")) %>%
  select(-group)

test<- data


test$out_prop[test$out_pprop>0.0499] <- 0 

df <- test
df$out_prop <- (df$out_prop)*100

df$X <- NULL
df[,5:15] <- NULL
df[,6] <- NULL

df1 <- df


##add a thing for indexing columns
d <- data.frame(domain = c(rep("thickness",151), rep("area", 151), rep("volume", 151)))
d$counter <- 1:453
n <- 40
df1 <- do.call("rbind", replicate(n, d, simplify = FALSE))

df_final <- merge(df, df1)


df$group[df$group==1:118, 3 ] <- "thickness"
df$group[df$group==2:119, 3] <- "area"
df$group[df$group==3:120, 3] <- "volume"
df$group <- as.factor(df$group)
df$out_delta[df$out_delta<0] <- 0



long <- melt(df, id.vars = c("independent_variable", "out_variable", "group"), measure.vars = c("out_delta"))

head(df)

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

library(grid)
library(gridExtra)
library(forestploter)

# Read meta analysis example data
dt <- read.csv(system.file("extdata", "metadata.csv", package = "forestploter"))
dt$cicol <- paste(rep(" ", 20), collapse = " ")

# Select some columns for plotting, this will be used as a skeleton of the forestplot
dt_fig <- dt[,c(1:7, 17, 8:13)]

colnames(dt_fig) <- c("Study or Subgroup",
                      "Events","Total","Events","Total",
                      "Weight",
                      "", "",
                      LETTERS[1:6])

dt_fig$Weight <- sprintf("%0.1f%%", dt_fig$Weight)
dt_fig$Weight[dt_fig$Weight == "NA%"] <- ""

# Convert NA to blank string
dt_fig[is.na(dt_fig)] <- ""

# Draw a forest plot ===========
# Define the theme
tm <- forest_theme(core = list(bg_params=list(fill = c("white"))),
                   summary_col = "black",
                   arrow_label_just = "end",
                   arrow_type = "closed")

p <- forest(dt_fig,
            est = dt$est,
            lower = dt$lb, 
            upper = dt$ub,
            sizes = sqrt(dt$weights/100),
            is_summary = c(rep(F, nrow(dt)-1), T),
            ci_column = 8,
            ref_line = 1,
            x_trans = "log",
            arrow_lab = c("Favours caffeine","Favours decaf"),
            xlim = c(0.05, 100),
            ticks_at = c(0.1, 1, 10, 100),
            theme = tm)
p

# Edit the forestplot =============
# Change fontface
g <- edit_plot(p, row = 9, 
               gp = gpar(fontface = "bold"))

# Change color
g <- edit_plot(g, col = 8, row = 9, which = "ci", 
              gp = gpar(col = "blue", fill = "blue"))

# Change background of the total row
g <- edit_plot(g, col = 1:7, row = 9, 
               which = "background", 
               gp = gpar(fill = "#f6eff7"))

# Align texts to center
g <- edit_plot(g, col = 9:14, which = "text",
               hjust = unit(0.5, "npc"),
               x = unit(0.5, "npc"))
g

# Add or insert text ==================
# Add or insert some text to header on top of CI columns
g <- add_text(g, text = "IV, Random, 95% CI", part = "header", col = 7:8, gp = gpar(fontface = "bold"))
g <- insert_text(g, text = "Odds ratio", part = "header", col = 7:8, gp = gpar(fontface = "bold"))

# Group outcomes
g <- add_text(g, text = "Caffeine", part = "header", row = 1, col = 2:3, gp = gpar(fontface = "bold"))
g <- add_text(g, text = "Decaf", part = "header", row = 1, col = 4:5, gp = gpar(fontface = "bold"))

# Add text on the top of risk of bias data
g <- add_text(g, text = "Risk of Bias", part = "header", row = 1, col = 9:14, gp = gpar(fontface = "bold"))

# Insert event count
g <- insert_text(g, text = c("Total events:"), row = 9, col = 1, before = FALSE, just = "left")

# Note: The row counts need to add one to account for `insert_text` in the previous step
g <- add_text(g, text = "58", col = 2, row = 10, just = "left")
g <- add_text(g, text = "46", col = 4, row = 10, just = "left")

g


# Add boder to a forest plot

# Add or insert some text to header
g <- add_border(g, part = "header", row = 1, col = 9:14,
                gp = gpar(lwd = .5))

g <- add_border(g, part = "header", row = 2,
                gp = gpar(lwd = 1))

g


# Add grobs to a forest plot
g <- add_grob(g, row = 1:c(nrow(dt_fig) - 1), col = 9:14, order = "backgroud",
              gb_fn = roundrectGrob, r = unit(0.05, "snpc"),
              gp = gpar(lty = "dotted", col = "#bdbdbd"))

# Draw circle grob, you can also draw a `pointsGrob`
cols <- c("#eeee00", "#00cc00", "#cc0000")
symb <- c("?", "+", "-")
for(i in seq_along(symb)){
  pos <- which(dt_fig == symb[i], arr.ind=TRUE)
  for(j in 1:nrow(pos)){
    g <- add_grob(g, row = pos[j,1], col = pos[j,2],
                  order = "backgroud", 
                  gb_fn = circleGrob, 
                  r = 0.4,
                  gp = gpar(fill = cols[i]))
  }
}

g

# Add grobs from other package to a forest plot
txt <- "Heterogeneity: &tau;<sup>2</sup> = 0.22; &chi;<sup>2</sup> = 9.39,
df=6 (P=0.15);I<sup>2</sup> = 36%<br><span style='color:blue'>**Total for overall effect:**</span> Z = 1.15(P=0.25)"

add_grob(g, row = 11, col = 1:6, 
         order = "backgroud",
         gb_fn = gridtext::richtext_grob,
         text = txt, 
         gp = gpar(fontsize = 8),
         hjust = 0, vjust = 1, 
         halign = 0, valign = 1,
         x = unit(0, "npc"), y = unit(1, "npc"))


# Multiple CI columns and multiple group ============
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
# indent the subgroup if there is a number in the placebo column
dt$Subgroup <- ifelse(is.na(dt$Placebo), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# NA to blank or NA will be transformed to carachter.
dt$n1 <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$n2 <- ifelse(is.na(dt$Placebo), "", dt$Placebo)

# Add two blank column for CI
dt$`CVD outcome` <- paste(rep(" ", 20), collapse = " ")
dt$`COPD outcome` <- paste(rep(" ", 20), collapse = " ")
dt <- dt[1:14, ]

# Set-up theme
tm <- forest_theme(base_size = 10,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c("#377eb8", "#4daf4a"),
                   footnote_col = "blue",
                   legend_name = "Group",
                   legend_value = c("Trt 1", "Trt 2"),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"))

p <- forest(dt[,c(1, 19, 21, 20, 22)],
            est = list(dt$est_gp1, dt$est_gp2, dt$est_gp3, dt$est_gp4),
            lower = list(dt$low_gp1, dt$low_gp2, dt$low_gp3, dt$low_gp4), 
            upper = list(dt$hi_gp1, dt$hi_gp2, dt$hi_gp3, dt$hi_gp4),
            ci_column = c(3, 5),
            ref_line = 1,
            vert_line = c(0.5, 2),
            nudge_y = 0.2,
            theme = tm)

plot(p)

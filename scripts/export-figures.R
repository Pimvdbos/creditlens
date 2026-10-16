# Recreate the portfolio chart from verified saved results; no model fitting.
source("submission/CreditLens.R")
assets <- creditlens("load")
scores <- assets$app$runtime$metrics
scores <- scores[scores$family != "baseline", ]
scores$label <- unname(model_labels[scores$family])
scores$label[scores$family == "lasso"] <- "LASSO (implemented)"
scores$label <- factor(scores$label, levels = rev(scores$label))

auc <- data.frame(
  label = scores$label, family = scores$family,
  metric = "Holdout AUC (higher is better)",
  value = scores$AUC, lower = scores$Lower, upper = scores$Upper
)
loss <- data.frame(
  label = scores$label, family = scores$family,
  metric = "Holdout log loss (lower is better)",
  value = scores$LogLoss, lower = NA_real_, upper = NA_real_
)
plot_data <- rbind(auc, loss)
plot_data$implemented <- plot_data$family == "lasso"

library(ggplot2)
chart <- ggplot(plot_data, aes(x = value, y = label, color = implemented)) +
  geom_linerange(
    data = auc, aes(xmin = lower, xmax = upper),
    orientation = "y", linewidth = 0.7, color = "#a9b7c7"
  ) +
  geom_point(size = 3.6) +
  geom_text(aes(label = sprintf("%.4f", value)), vjust = -1, size = 3.7) +
  facet_wrap(~metric, scales = "free_x", nrow = 1) +
  scale_color_manual(values = c("FALSE" = "#526579", "TRUE" = "#087f8c")) +
  scale_x_continuous(expand = expansion(mult = c(0.16, 0.16))) +
  scale_y_discrete(expand = expansion(add = 0.75)) +
  labs(
    x = NULL, y = NULL,
    title = "Holdout performance: the forest predicts better; LASSO was selected on development data",
    subtitle = "6,001 customers | AUC intervals: 95% | No holdout-based family selection",
    caption = paste("Frozen release", assets$identity$release_id)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none", panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15)),
    plot.caption = element_text(color = "#526579"),
    plot.margin = margin(16, 24, 12, 16)
  )

dir.create("docs/images", recursive = TRUE, showWarnings = FALSE)
ggsave(
  "docs/images/holdout-performance.png", chart,
  width = 12, height = 4.4, dpi = 160, bg = "white"
)
cat("Exported performance figure from", assets$identity$release_id, "\n")

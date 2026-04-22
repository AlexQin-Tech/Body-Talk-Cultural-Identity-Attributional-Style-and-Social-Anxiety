suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(patchwork)
  library(ggpubr)
})

file_path <- tcltk::tk_choose.files(
  caption = "请选择『Formal Sample.xlsx』",
  multi = FALSE
)
if (length(file_path) == 0) stop("未选择文件")
cat("正在读取：", basename(file_path), "\n")
data <- read_excel(file_path, sheet = 1)

data <- data %>%
  mutate(Gender = factor(Gender, levels = c(1, 2), labels = c("Male", "Female")))

n_male   <- sum(data$Gender == "Male", na.rm = TRUE)
n_female <- sum(data$Gender == "Female", na.rm = TRUE)
n_target <- min(n_male, n_female)
cat("原始样本量：男性 =", n_male, "，女性 =", n_female, "\n")
cat("均衡采样目标样本量：", n_target, "（每组）\n")

balanced_sample <- function(df, n_per_group) {
  df %>%
    group_by(Gender) %>%
    slice_sample(n = n_per_group, replace = FALSE) %>%
    ungroup()
}

original_pvals <- data %>%
  pivot_longer(cols = c(IP, EPP, ESP, IN, EPN, ESN),
               names_to = "Factor", values_to = "Score") %>%
  group_by(Factor) %>%
  summarise(
    p_original = wilcox.test(Score ~ Gender)$p.value,
    .groups = "drop"
  ) %>%
  mutate(Factor_label = factor(Factor,
                               levels = c("IP", "EPP", "ESP", "IN", "EPN", "ESN"),
                               labels = c("Internal\n(Positive)",
                                          "Ext-Personal\n(Positive)",
                                          "Ext-Situational\n(Positive)",
                                          "Internal\n(Negative)",
                                          "Ext-Personal\n(Negative)",
                                          "Ext-Situational\n(Negative)")))

set.seed(2025)
balanced_data <- balanced_sample(data, n_target)

balanced_pvals <- balanced_data %>%
  pivot_longer(cols = c(IP, EPP, ESP, IN, EPN, ESN),
               names_to = "Factor", values_to = "Score") %>%
  group_by(Factor) %>%
  summarise(
    p_balanced = wilcox.test(Score ~ Gender)$p.value,
    .groups = "drop"
  )

n_boot <- 1000
boot_results <- map_dfr(1:n_boot, ~{
  set.seed(2025 + .x)  # 不同种子
  boot_df <- balanced_sample(data, n_target)
  boot_df %>%
    pivot_longer(cols = c(IP, EPP, ESP, IN, EPN, ESN),
                 names_to = "Factor", values_to = "Score") %>%
    group_by(Factor) %>%
    summarise(p_boot = wilcox.test(Score ~ Gender)$p.value,
              .groups = "drop") %>%
    mutate(boot_id = .x)
})

boot_summary <- boot_results %>%
  group_by(Factor) %>%
  summarise(
    p_boot_median = median(p_boot),
    p_boot_lower  = quantile(p_boot, 0.025),
    p_boot_upper  = quantile(p_boot, 0.975),
    .groups = "drop"
  )

pval_compare <- original_pvals %>%
  left_join(balanced_pvals, by = "Factor") %>%
  left_join(boot_summary, by = "Factor") %>%
  mutate(Factor_label = factor(Factor,
                               levels = c("IP", "EPP", "ESP", "IN", "EPN", "ESN"),
                               labels = c("Internal\n(Positive)",
                                          "Ext-Personal\n(Positive)",
                                          "Ext-Situational\n(Positive)",
                                          "Internal\n(Negative)",
                                          "Ext-Personal\n(Negative)",
                                          "Ext-Situational\n(Negative)")))

cat("\n=== p ===\n")
print(pval_compare %>% select(Factor_label, p_original, p_balanced, p_boot_median, p_boot_lower, p_boot_upper))

plot_data <- balanced_data %>%
  select(Gender, IP, EPP, ESP, IN, EPN, ESN) %>%
  pivot_longer(cols = c(IP, EPP, ESP, IN, EPN, ESN),
               names_to = "Factor",
               values_to = "Score") %>%
  mutate(Factor = factor(Factor,
                         levels = c("IP", "EPP", "ESP", "IN", "EPN", "ESN"),
                         labels = c("Internal\n(Positive)",
                                    "Ext-Personal\n(Positive)",
                                    "Ext-Situational\n(Positive)",
                                    "Internal\n(Negative)",
                                    "Ext-Personal\n(Negative)",
                                    "Ext-Situational\n(Negative)")))

gender_cols <- c("Male" = "#2C7BB6", "Female" = "#D7191C")

plot_factor <- function(this_factor) {
  df <- plot_data %>% filter(Factor == this_factor)
  max_y <- max(df$Score, na.rm = TRUE)
  
  p_val <- wilcox.test(Score ~ Gender, data = df)$p.value
  
  stars <- ifelse(p_val < 0.001, "***",
                  ifelse(p_val < 0.01, "**",
                         ifelse(p_val < 0.05, "*", "")))
  
  p_text <- ifelse(p_val < 0.001, "p < .001",
                   sprintf("p = %.3f", round(p_val, 3)))
  
  ggplot(df, aes(x = Gender, y = Score, fill = Gender)) +
    geom_violin(trim = FALSE, alpha = 0.92, colour = "black", linewidth = 0.7) +
    geom_boxplot(width = 0.12, outlier.shape = NA, colour = "black", fill = "white", size = 0.8) +
    scale_fill_manual(values = gender_cols) +
    
    { if(stars != "") annotate("text", x = 1.5, y = max_y * 1.14,
                               label = stars, size = 6, fontface = "bold") } +
    
    annotate("text", x = 1.5, y = max_y * 1.06,
             label = p_text, size = 6) +
    
    labs(title = this_factor, x = "", y = "Attribution Score") +
    theme_classic(base_size = 14) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    coord_cartesian(clip = "off")
}

p1 <- plot_factor("Internal\n(Positive)")
p2 <- plot_factor("Ext-Personal\n(Positive)")
p3 <- plot_factor("Ext-Situational\n(Positive)")
p4 <- plot_factor("Internal\n(Negative)")
p5 <- plot_factor("Ext-Personal\n(Negative)")
p6 <- plot_factor("Ext-Situational\n(Negative)")

final <- (p1 | p2 | p3) / (p4 | p5 | p6) +
  plot_annotation(
    title = "Gender Differences in Attribution Scores (Balanced Sampling)",
    tag_levels = "A",
    caption = paste0("*p < .05, **p < .01, ***p < .001 (Wilcoxon rank-sum test)\n",
                     "Balanced sampling：n = ", n_target, " per group (random subsampling)")
  ) &
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.tag = element_text(size = 20, face = "bold"),
    plot.caption = element_text(hjust = 1, size = 12, face = "italic",
                                colour = "grey30", margin = margin(t = 20))
  )

unlink("Attribution.png")
unlink("Attribution.pdf")
ggsave("Attribution.png", final,
       width = 16, height = 10, dpi = 600, bg = "white")
ggsave("Attribution.pdf", final,
       width = 16, height = 10, dpi = 600, bg = "white")

print(final)

cat("\n1\n")
cat("2\n")
cat("3\n")
cat("4\n")
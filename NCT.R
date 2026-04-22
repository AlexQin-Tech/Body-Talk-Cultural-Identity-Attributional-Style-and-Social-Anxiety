pkgs <- c("readxl", "dplyr", "qgraph", "bootnet", "NetworkComparisonTest", "tcltk")
new <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, dependencies = TRUE)

library(readxl)
library(dplyr)
library(qgraph)
library(bootnet)
library(NetworkComparisonTest)
library(tcltk)

cat("请在弹窗中选择你的xlsx数据文件……\n")
file_path <- tcltk::tk_choose.files(
  caption = "请选择xlsx文件",
  filters = matrix(c("Excel", ".xlsx;.xls"), 2, 2, byrow = TRUE)
)

if (length(file_path) == 0) stop("未选择文件")

data <- read_excel(file_path)

vars <- c("ACis", "ACac", "BC", "NFT", "NMTa", "NMTb", "PBTi", "PBTe",
          "EB", "PB", "P", "SI", "IBI", "PA")

data14 <- data %>%
  select(all_of(vars), Gender) %>%
  na.omit()

male <- data14 %>% filter(Gender == 1) %>% select(all_of(vars))
female <- data14 %>% filter(Gender == 2) %>% select(all_of(vars))

cat("男生 n =", nrow(male), "  女生 n =", nrow(female), "\n\n")

set.seed(2025)
net_m <- estimateNetwork(male, default = "EBICglasso", tuning = 0.25, corMethod = "cor_auto")
net_f <- estimateNetwork(female, default = "EBICglasso", tuning = 0.25, corMethod = "cor_auto")

cat("Negative edges proportion:\n")
cat("  Male:  ", round(mean(net_m$graph < 0) * 100, 2), "%\n")
cat("  Female:", round(mean(net_f$graph < 0) * 100, 2), "%\n\n")

cat("正在运行 NCT（5000次置换）...\n")
nct <- NCT(net_m, net_f,
           it = 5000,
           test.edges = TRUE,
           test.centrality = TRUE,
           edges = "all",
           progressbar = TRUE)

png("Gender_Network_Comparison_Main.png", width = 5200, height = 2600, res = 360)

layout(matrix(c(1, 2), ncol = 2), widths = c(1, 1))
par(mar = c(2, 2, 6, 2))

strength_m <- colSums(abs(net_m$graph))
strength_f <- colSums(abs(net_f$graph))
vsize_m <- 5 + 6 * (strength_m - min(strength_m)) / (max(strength_m) - min(strength_m) + 1e-10)
vsize_f <- 5 + 6 * (strength_f - min(strength_f)) / (max(strength_f) - min(strength_f) + 1e-10)

node_colors <- c(rep("#2c7bb6", 3), rep("#fdae61", 5), rep("#d7191c", 2), rep("#abdda4", 4))
names(node_colors) <- vars

qgraph(net_m$graph,
       layout = "spring",
       vsize = vsize_m,
       color = node_colors,
       label.cex = 1.15,
       label.color = "white",
       font = 2,
       border.width = 1.6,
       edge.color = ifelse(net_m$graph > 0, "#003366", "#e31a1c"),
       esize = 18,
       title = paste0("Male Network (n = ", nrow(male), ")"),
       title.cex = 1.6)

qgraph(net_f$graph,
       layout = "spring",
       vsize = vsize_f,
       color = node_colors,
       label.cex = 1.15,
       label.color = "white",
       font = 2,
       border.width = 1.6,
       edge.color = ifelse(net_f$graph > 0, "#003366", "#e31a1c"),
       esize = 18,
       title = paste0("Female Network (n = ", nrow(female), ")"),
       title.cex = 1.6)

dev.off()
cat("图1 已保存：Gender_Network_Comparison_Main.png\n")

png("Global_Strength_Difference.png", width = 3000, height = 2400, res = 360)
plot(nct, what = "strength")
dev.off()

png("Edge_Difference_Heatmap.png", width = 3400, height = 3000, res = 360)
plot(nct, what = "edge", layout = "spring")
dev.off()
cat("图2、图3 已保存\n")

cent_m <- centrality(net_m$graph)
cent_f <- centrality(net_f$graph)

cent_all <- data.frame(
  Node = vars,
  Strength_m = colSums(abs(net_m$graph)),
  Strength_f = colSums(abs(net_f$graph)),
  Closeness_m = cent_m$Closeness,
  Closeness_f = cent_f$Closeness,
  Betweenness_m = cent_m$Betweenness,
  Betweenness_f = cent_f$Betweenness,
  ExpectedInfluence_m = colSums(net_m$graph),
  ExpectedInfluence_f = colSums(net_f$graph)
)

cent_z <- cent_all %>%
  arrange(desc(ExpectedInfluence_m + ExpectedInfluence_f)) %>%
  mutate(across(ends_with("_m"), ~ as.numeric(scale(.))),
         across(ends_with("_f"), ~ as.numeric(scale(.)))) %>%
  mutate(Node = factor(Node, levels = Node))

png("Centrality_SideBySide_4Panels.png", width = 5600, height = 4800, res = 360)
layout(matrix(1:4, 2, 2, byrow = TRUE))
par(mar = c(8, 6, 4, 3), cex.main = 2.2)

cols <- c(Male = "#1f77b4", Female = "#ff7f0e")
pchs <- c(Male = 16, Female = 15)

for (i in c("Strength", "Closeness", "Betweenness", "ExpectedInfluence")) {
  m_col <- paste0(i, "_m")
  f_col <- paste0(i, "_f")
  
  ylim_range <- range(c(cent_z[[m_col]], cent_z[[f_col]]), na.rm = TRUE) + c(-0.4, 0.6)
  
  plot(1:nrow(cent_z), cent_z[[m_col]],
       type = "b", col = cols["Male"], pch = pchs["Male"], lwd = 3.5,
       ylim = ylim_range, xaxt = "n", ylab = "Centrality (z-score)",
       main = i, cex.lab = 1.8, cex.axis = 1.4, xlab = "")
  
  lines(1:nrow(cent_z), cent_z[[f_col]],
        type = "b", col = cols["Female"], pch = pchs["Female"], lwd = 3.5)
  
  axis(1, at = 1:nrow(cent_z), labels = levels(cent_z$Node), las = 2, cex.axis = 1.35)
  grid(nx = NA, ny = NULL)
  abline(h = 0, lty = 2, col = "gray50")
  legend("topright", legend = c("Male", "Female"),
         col = cols, pch = pchs, lwd = 3.5, cex = 1.6, bg = "white")
}
dev.off()
cat("图4 已保存：Centrality_SideBySide_4Panels.png\n")

cat("\n========== NCT Global Results Summary ==========\n")
cat("1. Global Strength Invariance p-value =", round(nct$glstrinv.pval, 4), "\n")
cat("2. Network Invariance p-value (overall structure) =", round(nct$nwinv.pval, 4), "\n")
cat("3. Number of Significant Edges =", sum(nct$einv.pvals$`p-value` < 0.05, na.rm = TRUE), "\n")

global_results <- data.frame(
  Item = c("Global Strength Invariance p-value",
           "Network Invariance p-value (overall structure)",
           "Number of Significant Edges"),
  Value = c(round(nct$glstrinv.pval, 4),
            round(nct$nwinv.pval, 4),
            sum(nct$einv.pvals$`p-value` < 0.05, na.rm = TRUE))
)

write.csv(global_results, "NCT_Global_Results.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("1\n")

cat("\n2\n")
cat("3", getwd(), "\n")
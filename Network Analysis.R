pkgs <- c("readxl","dplyr","tidyr","qgraph","bootnet","networktools","psych","tcltk")
new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new, dependencies = TRUE)

library(readxl); library(dplyr); library(tidyr); library(qgraph)
library(bootnet); library(networktools); library(psych); library(tcltk)

cat("请在弹窗中选择你的xlsx数据文件……\n")
file_path <- tcltk::tk_choose.files(caption="请选择xlsx文件",
                                    filters=matrix(c("Excel",".xlsx;.xls"),2,2,byrow=TRUE))
if(length(file_path)==0) stop("未选择文件，程序终止")

data <- read_excel(file_path)
cat("成功读取：", basename(file_path), "\n")

vars <- c("ACis","ACac","BC",
          "NFT","NMTa","NMTb","PBTi","PBTe",
          "EB","PB",
          "P","SI","IBI","PA")

if(!all(vars %in% names(data)))
  stop("变量名错误：", paste(vars[!vars %in% names(data)], collapse=", "))

data14 <- data %>% select(all_of(vars)) %>% na.omit()
n_nodes <- length(vars)
n <- nrow(data14)
cat("有效样本量：", n, "\n\n")

data14_std <- data14
for (col in vars) {
  data14_std[[col]] <- (data14[[col]] - mean(data14[[col]], na.rm=TRUE)) / 
    sd(data14[[col]], na.rm=TRUE)
}

set.seed(2025)
network <- estimateNetwork(data14_std, default="EBICglasso", tuning=0.25, corMethod="cor_auto")

cat("Negative edges proportion in the network:\n")
neg_prop <- mean(network$graph < 0) * 100
cat("  → ", round(neg_prop, 2), "% of all edges are negative\n")
cat("(Method/Results)\n\n")

cat("正在生成描述性统计...\n")

descriptives_raw <- data14 %>%
  summarise(across(all_of(vars),
                   list(Mean=~mean(.,na.rm=TRUE), SD=~sd(.,na.rm=TRUE),
                        Min=~min(.,na.rm=TRUE), Max=~max(.,na.rm=TRUE),
                        Skew=~psych::skew(.,na.rm=TRUE), Kurt=~psych::kurtosi(.,na.rm=TRUE)),
                   .names="{.col}_{.fn}")) %>%
  pivot_longer(everything()) %>% separate(name,c("Node","Stat"),"_") %>%
  pivot_wider(names_from=Stat, values_from=value) %>%
  mutate(across(where(is.numeric), ~round(.,3)))

descriptives_std <- data14_std %>%
  summarise(across(everything(),
                   list(Mean=~mean(.,na.rm=TRUE), SD=~sd(.,na.rm=TRUE),
                        Min=~min(.,na.rm=TRUE), Max=~max(.,na.rm=TRUE),
                        Skew=~psych::skew(.,na.rm=TRUE), Kurt=~psych::kurtosi(.,na.rm=TRUE)),
                   .names="{.col}_{.fn}")) %>%
  pivot_longer(everything()) %>% separate(name,c("Node","Stat"),"_") %>%
  pivot_wider(names_from=Stat, values_from=value) %>%
  mutate(across(where(is.numeric), ~round(.,3)))

write.csv(descriptives_raw, "Node_Descriptive_Statistics_RAW.csv", row.names=FALSE, fileEncoding="UTF-8")
write.csv(descriptives_std, "Node_Descriptive_Statistics_Standardized.csv", row.names=FALSE, fileEncoding="UTF-8")

edge_matrix <- round(network$graph, 4); diag(edge_matrix) <- 0
rownames(edge_matrix) <- colnames(edge_matrix) <- vars
write.csv(edge_matrix, "Edge_Weight_Matrix.csv", fileEncoding="UTF-8")

g <- network$graph
centrality_obj <- qgraph::centrality(g)
cent_table <- data.frame(
  Node = vars,
  Strength = round(colSums(abs(g)), 4),
  Closeness = round(centrality_obj$Closeness, 4),
  Betweenness = round(centrality_obj$Betweenness, 4),
  ExpectedInfluence = round(colSums(g), 4)
) %>% arrange(desc(ExpectedInfluence))
write.csv(cent_table, "Centrality_Indices_Table.csv", row.names=FALSE, fileEncoding="UTF-8")

Strength_raw <- colSums(abs(g))
Closeness_raw <- centrality_obj$Closeness
Betweenness_raw <- centrality_obj$Betweenness
ExpectedInfluence_raw <- colSums(g)

cent_z <- scale(cbind(Strength_raw, Closeness_raw, Betweenness_raw, ExpectedInfluence_raw))
colnames(cent_z) <- c("Strength","Closeness","Betweenness","ExpectedInfluence")
rownames(cent_z) <- vars

node_order <- order(cent_z[,"ExpectedInfluence"], decreasing = TRUE)
cent_z <- cent_z[node_order, ]

png("Centrality_Plot_4Panels.png", width=4800, height=4400, res=360)
layout(matrix(1:4, 2, 2, byrow = TRUE))
par(mar=c(6, 4.5, 4, 2), cex.main=1.8)

colors <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728")
pchs <- c(16,15,17,18)
titles <- c("Strength","Closeness","Betweenness","Expected Influence")

for(i in 1:4){
  plot(1:n_nodes, cent_z[,i], type="b", pch=pchs[i], col=colors[i], lwd=3.5,
       ylim=range(cent_z)+c(-0.4,0.7), xaxt="n", xlab="", ylab="Centrality (z-score)",
       main=titles[i], cex.main=2.2, cex.lab=1.5)
  axis(1, at=1:n_nodes, labels=rownames(cent_z), las=2, cex.axis=1.15)
  grid(col="lightgray", lty="dotted")
}
dev.off()
cat("V2：Centrality_Plot_4Panels.png\n")

png("Network_Diagram_Main.png", width = 4800, height = 2700, res = 360)
layout(matrix(c(1,2), ncol=2), widths = c(6.8, 3.2))
par(mar = c(1, 2, 6, 0))

EI <- colSums(network$graph)
vsize <- 5 + 6 * (EI - min(EI)) / (max(EI) - min(EI) + 1e-10)

node_colors <- c(rep("#2c7bb6",3), rep("#fdae61",5), rep("#d7191c",2), rep("#abdda4",4))
names(node_colors) <- vars

qgraph(network$graph,
       layout = "spring",
       vsize = vsize,
       nodeNames = vars,
       color = node_colors,
       label.cex = 1.10,
       label.color = "white",
       font = 2,
       cut = 0,
       negDashed = FALSE,
       edge.color = ifelse(network$graph > 0, "#003366", "#e31a1c"),
       border.width = 1.6,
       border.color = "black",
       esize = 16,
       maximum = max(abs(network$graph)),
       legend = FALSE,
       title = paste0("Regularized Partial Correlation Network (N = ", n, ")"),
       title.cex = 1.6)

par(mar = c(0,0,0,0))
plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))
text(0.05, 0.96, "Node Legend", adj=0, cex=1.2, font=2)

labels <- c(
  "ACis" = "Affective Commitment (Ingroup Satisfaction)",
  "ACac" = "Affective Commitment (Attachment-Centrality)",
  "BC" = "Behavioral Commitment",
  "NFT" = "Negative Fat Talk",
  "NMTa" = "Negative Muscle Talk (Affective)",
  "NMTb" = "Negative Muscle Talk (Behavioral)",
  "PBTi" = "Positive Body Talk (Internal)",
  "PBTe" = "Positive Body Talk (External)",
  "EB" = "Externalizing Bias",
  "PB" = "Personalizing Bias",
  "P" = "Performance",
  "SI" = "Social Interaction",
  "IBI" = "Ice-breaking Interaction",
  "PA" = "Participation"
)

y0 <- 0.89
for(i in seq_along(vars)){
  y <- y0 - (i-1)*0.0575
  text(0.05, y, labels[vars[i]], adj=0, cex=0.8, col="black")
  rect(0.012, y - 0.008, 0.042, y + 0.022, col=node_colors[vars[i]], border="gray40")
}
legend(0.01, 0.08, legend=c("Positive edge", "Negative edge"),
       col=c("#003366","#e31a1c"), lwd=5, cex=0.9, bty="n", horiz=TRUE)
dev.off()
cat("V1\n")

cat("正在计算桥中心性...\n")
g <- network$graph
rownames(g) <- colnames(g) <- vars

community <- c(
  ACis="Culture", ACac="Culture", BC="Culture",
  NFT="BodyTalk", NMTa="BodyTalk", NMTb="BodyTalk", PBTi="BodyTalk", PBTe="BodyTalk",
  EB="Attribution", PB="Attribution",
  P="SocAnx", SI="SocAnx", IBI="SocAnx", PA="SocAnx"
)

bridge_strength <- numeric(n_nodes)
bridge_EI <- numeric(n_nodes)
names(bridge_strength) <- names(bridge_EI) <- vars

for (node in vars) {
  my_comm <- community[node]
  cross_edges <- g[node, community != my_comm]
  bridge_strength[node] <- sum(abs(cross_edges))
  bridge_EI[node]       <- sum(cross_edges)
}

bridge_table <- data.frame(
  Node = vars,
  Bridge_Strength = round(bridge_strength, 4),
  Bridge_Expected_Influence = round(bridge_EI, 4),
  Community = community
) %>% arrange(desc(Bridge_Expected_Influence))

write.csv(bridge_table, "Bridge_Centrality_Table.csv", row.names=FALSE, fileEncoding="UTF-8")

png("Bridge_Centrality.png", width=4600, height=2800, res=360)
z1 <- scale(bridge_table$Bridge_Strength)
z2 <- scale(bridge_table$Bridge_Expected_Influence)
ord <- order(z2, decreasing=TRUE)
par(mar=c(8,4.5,4,2))
plot(1:n_nodes, z1[ord], type="n", xaxt="n", ylim=range(c(z1,z2))+c(-0.5,0.6),
     xlab="", ylab="Bridge Centrality (z-score)", main="Bridge Centrality Indices", cex.main=2)
axis(1, at=1:n_nodes, labels=bridge_table$Node[ord], las=2, cex.axis=1.2)
lines(1:n_nodes, z1[ord], type="b", pch=16, col="#1f77b4", lwd=3.5)
lines(1:n_nodes, z2[ord], type="b", pch=18, col="#d62728", lwd=3.5)
legend("topright", legend=c("Bridge Strength","Bridge Expected Influence"),
       col=c("#1f77b4","#d62728"), pch=c(16,18), lwd=3, cex=1.4, bty="n")
grid(col="lightgray", lty="dotted")
dev.off()
cat("V3\n")

cat("正在计算边权重稳定性（5000次）...\n")
set.seed(2025)
boot_edge <- bootnet(network, boots=5000, type="nonparametric", statistics = c("edge","strength","expectedInfluence"))
png("Edge_Stability.png", width=4200, height=2600, res=360)
plot(boot_edge, labels=FALSE, order="sample")
dev.off()

png("Centrality_Difference_Test.png", width=4200, height=5200, res=360)
par(mfrow=c(2,1))
plot(boot_edge, "strength")
plot(boot_edge, "expectedInfluence")
dev.off()

cat("正在计算中心性稳定性（2500次）...\n")
set.seed(2025)
stab_case <- bootnet(network, boots=2500, type="case",
                     statistics = c("strength","closeness","betweenness","expectedInfluence"))
png("Centrality_Stability.png", width=4200, height=2600, res=360)
plot(stab_case, statistics = c("strength","expectedInfluence","closeness","betweenness"))
dev.off()

cs <- corStability(stab_case)
cat("\nCS-coefficient (correlation stability, 95% certainty):\n")
print(cs)

cat("\n1\n")
cat("2", getwd(), "\n")
cat("3\n")
cat("4\n")
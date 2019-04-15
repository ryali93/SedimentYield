# rm(list=ls())
library(dplyr)
library(reshape2)
library(ggplot2)

df = read.csv("E:/TESIS/process/results.csv", sep = ",", header = T)

df2 = df %>% filter(ts != "1981-01-01" & ts != "1982-01-01" & ts != "1983-01-01" & ts != "1985-01-01" & ts != "2016-01-01")
df3 = df2 %>% select(-r_c3_ls1_sdr1, -r_c3_ls1_sdr2, -r_c3_ls1_sdr3, -r_c3_ls1_sdr4, -r_c3_ls2_sdr1, -r_c3_ls2_sdr2, -r_c3_ls2_sdr3, -r_c3_ls2_sdr4,
                    -r_c3_ls3_sdr1, -r_c3_ls3_sdr2, -r_c3_ls3_sdr3, -r_c3_ls3_sdr4, -X, -ts)
# meltdf <- melt(df3, id="ts")
# ggplot(meltdf, aes(x=ts, y=value,colour=variable,group=variable)) + geom_line()

df4 = df3/(1.65*1000000*7.5)

anos = data.frame(seq(as.Date("1984-01-01"), as.Date("2015-01-01"), by="year"))
names(anos) = "anos"
anos = filter(anos, anos != "1985-01-01")
df5 = cbind(anos, df4)

PEJEZA = c(NA, NA, NA, 2, 3, 4, 5, 11, 16, 17, 21, 22, 60, 62, 63, 64, NA, NA, NA, NA, 80, 82, NA, NA, NA, NA, NA, NA, 104.53,
NA, NA)

df6 = cbind(df5, PEJEZA)

results = df6 %>% select(r_c5_ls3_sdr4,
                         r_c2_ls3_sdr4,
                         r_c4_ls1_sdr1,
                         r_c6_ls1_sdr4,
                         r_c6_ls3_sdr2,
                         PEJEZA)

results = cbind(anos, results)

results
plot(results)

#--------------------------------------------------------
df = read.csv("E:/TESIS/results/results2_missing.csv", sep = ";", header = T)
anos = data.frame(seq(as.Date("1988-01-01"), as.Date("2015-01-01"), by="year"))
names(anos) = "anos"
df2 = cbind(anos, df)

meltdf <- melt(df2, id="anos")
ggplot(meltdf, aes(x=anos, y=value, colour=variable, group=variable))
+ geom_line()
+ geom_boxplot(alpha = 0)

ggplot(meltdf,aes(x=value, y=variable, group=variable))+
  geom_bar(stat="identity", position="dodge")


ggplot(meltdf, aes(x = anos, y=value, group=variable))+
  geom_boxplot()



install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

chart.Correlation(df, histogram=TRUE, pch=19)

library(corrplot)
corrplot(df)

df
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cor(df), col = col, symm = TRUE)

corr = cor(df, method = c("pearson", "kendall", "spearman"))
corrplot(corr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


p <- ggplot(data = df2, aes(x = anos, y = mortes, group=interaction(date, trmt)))
p +  geom_boxplot(aes(fill = factor(dtm$trmt)))

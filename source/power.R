# Project: JFK Stroke Recovery Program
# Description: sample size calculation
# Author: Davit Sargsyan, Javier Cabrera
# Date: 06/03/2016
#****************************************************
# setwd("C:/Users/ds752/Documents/git_local/jfk.cvi")
require(data.table)
require(pwr)
require(bit64)
require(ggplot2)

# Data----
d1 <- fread(input = "data\\Demographics_20160422.csv",
            sep = ",")

d2 <- fread(input = "data\\Measurements_20160422.csv",
            sep = ",")

dd <- data.table(`Medical Record No` = d2$`Medical Record No`,
                 Group = d2$Group)
dd <- unique(dd)
setkey(dd)
dd

dd <- subset(dd, dd$Group != "No")
dd <- merge(dd, d1, by = "Medical Record No")
dd
summary(dd$Age)
table(dd$`Deceased_Y/N`, dd$Group)
table(dd$Gender, dd$Group)
table(dd$`Health Insurance Name`)

#****************************************************
d2 <- merge(d2, d1, by = "Medical Record No")

d2$`Medical Record No` <- factor(d2$`Medical Record No`)
setkey(d2, DaysId)
setkey(d2, `Medical Record No`)
d2[, trt := Group[nchar(Group) == max(nchar(Group), na.rm = TRUE)],
   by = `Medical Record No`]
d2
d2$trt <- factor(d2$trt,
                 levels = c("Study Group",
                            "Control Group",
                            "No"))

#****************************************************
# 1. PT-AM-PAC Basic Mobility Score----
d2$`PT-AM-PAC Basic Mobility Score`[d2$`PT-AM-PAC Basic Mobility Score` == 9999] <- NA
tmp <- droplevels(subset(d2, !is.na(d2$`PT-AM-PAC Basic Mobility Score`)))

ggplot(data = tmp, 
       aes(x = DaysId,
           y = `PT-AM-PAC Basic Mobility Score`,
           group = `Medical Record No`,
           colour = `Medical Record No`)) +
  facet_wrap(~ trt,
             nrow = 2) +
  geom_point(size = 2,
             alpha = 0.6,
             position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) + 
  scale_x_discrete("Visit Number") + 
  scale_y_continuous("PT-AM-PAC Basic Mobility Score") + 
  ggtitle("") +
  guides(colour = guide_legend(title = "Subject ID",
                               title.position = "top",
                               nrow = 4)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 0, hjust = 1))

ggplot(data = tmp, 
       aes(x = DaysId,
           y = `PT-AM-PAC Basic Mobility Score`,
           group = `Medical Record No`,
           colour = Gender)) +
  facet_wrap(~ trt,
             nrow = 2) +
  geom_point(size = 2,
             alpha = 0.6,
             position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) + 
  scale_x_discrete("Visit Number") + 
  scale_y_continuous("PT-AM-PAC Basic Mobility Score") + 
  ggtitle("") +
  guides(colour = guide_legend(title = "Gender",
                               title.position = "top",
                               nrow = 1)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_colour_manual(values = c("red", "blue"))

ggplot(data = tmp, 
       aes(x = DaysId,
           y = `PT-AM-PAC Basic Mobility Score`,
           group = `Medical Record No`,
           colour = Age)) +
  facet_wrap(~ trt,
             nrow = 2) +
  geom_point(size = 2,
             alpha = 0.6,
             position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) + 
  scale_x_discrete("Visit Number") + 
  scale_y_continuous("PT-AM-PAC Basic Mobility Score") + 
  scale_colour_gradient(low ="red", high = "blue")

#****************************************************
# 2. OT-AM-Daily Activity Score----
d2$`OT-AM-Daily Activity Score`[d2$`OT-AM-Daily Activity Score` == 9999] <- NA
tmp <- droplevels(subset(d2, !is.na(d2$`OT-AM-Daily Activity Score`)))

ggplot(data = tmp, 
       aes(x = DaysId,
           y = `OT-AM-Daily Activity Score`,
           group = `Medical Record No`,
           colour = `Medical Record No`)) +
  facet_wrap(~ trt,
             nrow = 2) +
  geom_point(size = 2,
             alpha = 0.6,
             position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) + 
  scale_x_discrete("Visit Number") + 
  scale_y_continuous("OT-AM-Daily Activity Score") + 
  ggtitle("") +
  guides(colour = guide_legend(title = "Subject ID",
                               title.position = "top",
                               nrow = 4)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 0, hjust = 1))

#****************************************************
# 3. ST-AM-Applied Cogn Score----
d2$`ST-AM-Applied Cogn Score`[d2$`ST-AM-Applied Cogn Score` == 9999] <- NA
tmp <- droplevels(subset(d2, !is.na(d2$`ST-AM-Applied Cogn Score`)))

ggplot(data = tmp, 
       aes(x = DaysId,
           y = `ST-AM-Applied Cogn Score`,
           group = `Medical Record No`,
           colour = `Medical Record No`)) +
  facet_wrap(~ trt,
             nrow = 2) +
  geom_point(size = 2,
             alpha = 0.6,
             position = position_dodge(0.3)) + 
  geom_line(position = position_dodge(0.3)) + 
  scale_x_discrete("Visit Number") + 
  scale_y_continuous("ST-AM-Applied Cogn Score") + 
  ggtitle("") +
  guides(colour = guide_legend(title = "Subject ID",
                               title.position = "top",
                               nrow = 4)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 0, hjust = 1))

#****************************************************
# 4. MoCA Score----
addmargins(table(d2$`MoCA Score`))
# d2$`MoCA Score`[d2$`MoCA Score` == 9999] <- NA
# tmp <- droplevels(subset(d2, !is.na(d2$`MoCA Score`)))
# 
# ggplot(data = tmp, 
#        aes(x = DaysId,
#            y = `MoCA Score`,
#            group = `Medical Record No`,
#            colour = `Medical Record No`)) +
#   facet_wrap(~ trt,
#              nrow = 2) +
#   geom_point(size = 4,
#              alpha = 0.6) + 
#   geom_line(size = 1.1) + 
#   scale_x_discrete("Visit Number") + 
#   scale_y_continuous("MoCA Score") + 
#   ggtitle("") +
#   guides(colour = guide_legend(title = "Subject ID",
#                                title.position = "top",
#                                nrow = 4)) +
#   theme(legend.position = "top",
#         axis.text.x = element_text(angle = 0, hjust = 1))

#****************************************************
# 5. ModRankinScore----
addmargins(table(d2$ModRankinScore))
# d2$ModRankinScore[d2$`ModRankinScore` == 9999] <- NA
# tmp <- droplevels(subset(d2, !is.na(d2$`ModRankinScore`)))
# 
# ggplot(data = tmp, 
#        aes(x = DaysId,
#            y = `ModRankinScore`,
#            group = `Medical Record No`,
#            colour = `Medical Record No`)) +
#   facet_wrap(~ trt,
#              nrow = 2) +
#   geom_point(size = 4,
#              alpha = 0.6) + 
#   geom_line(size = 1.1) + 
#   scale_x_discrete("Visit Number") + 
#   scale_y_continuous("ModRankinScore") + 
#   ggtitle("") +
#   guides(colour = guide_legend(title = "Subject ID",
#                                title.position = "top",
#                                nrow = 4)) +
#   theme(legend.position = "top",
#         axis.text.x = element_text(angle = 0, hjust = 1))

#****************************************************
#****************************************************
# Power Curves----
# N1 <- 964
# n1 <- 7
# N2 <- 1431
# n2 <- 21
# xx <- seq(100, 5000, by = 50)

N1 <- 1000
n1 <- 250
N2 <- 1000
n2 <- 150

N1 <- 1000
n1 <- 30
N2 <- 1000
n2 <- 27

N1 <- 1000
n1 <- 390
N2 <- 1000
n2 <- 351

h <- ES.h(p1 = n1/N1,
          p2 = n2/N2)

xx <- seq(10, 500, by = 10)

# Aplha = 0.05
out <- pwr.2p.test(h = h,
                   n = xx,
                   sig.level = 0.05,
                   alternative = "g")

plot(out$power ~ xx,
     type = "l",
     col = "red",
     xlab = "Number of Subjects Per Treatment Group",
     ylab = "Power",
     main = paste("Power Curves for a Variable with \n", 
                  n1/10,
                  "% Events in Control vs. ",
                  n2/10,
                  "% in Active Groups",
                  sep = ""),
     ylim = c(0, 1))
abline(h = 0.8,
       lty = 2)
nn <- xx[out$power == min(out$power[out$power >= 0.8])]
abline(v = nn,
       lty = 3)
text(x = nn - 5,
     labels = paste("N =",
                    nn),
     y = 0.4,
     srt = 90)

# Alpha = 0.01
out <- pwr.2p.test(h = h,
                   n = xx,
                   sig.level = 0.01,
                   alternative = "g")
lines(out$power ~ xx,
     col = "blue")
nn <- xx[out$power == min(out$power[out$power >= 0.8])]
abline(v = nn,
       lty = 3)
text(x = nn - 5,
     labels = paste("N =",
                    nn),
     y = 0.4,
     srt = 90)

# Alpha = 0.1
out <- pwr.2p.test(h = h,
                   n = xx,
                   sig.level = 0.1,
                   alternative = "g")
lines(out$power ~ xx,
      col = "green")
nn <- xx[out$power == min(out$power[out$power >= 0.8])]
abline(v = nn,
       lty = 3)
text(x = nn - 5,
     labels = paste("N =",
                    nn),
     y = 0.4,
     srt = 90)

legend("bottomright",
       legend = paste("Alpha = ",
                      c(0.1, 0.05, 0.01)),
       col = c("green", "red", "blue"),
       lty = 1)

#****************************************************
# Javier's method----

# k = 1000
# p1 <- rbinom(size = k,
#              n = 5000,
#              prob = 0.25)/k
# p2 <- rbinom(size = k,
#              n = 5000,
#              prob = 0.15)/k
# 
# foo <- function(pp1, pp2, kk = 1000) {
#   dm <- cbind(c(pp1,
#                 1 - pp1),
#               c(pp2,
#                 1 - pp2))*kk
#   out <- chisq.test(dm)$p.v
# }
# f1 <- Vectorize(foo, vectorize.args = c("pp1", "pp2"))
# out <- f1(p1, p2)
# mean(out < 0.05)
# hist(out, 100)

foo <- function(k, pp1, pp2, alpha) {
  p1 <- rbinom(k, n = 5000, p = pp1)/k
  p2 <- rbinom(k, n = 5000, p = pp2)/k
  res <- NULL
  for (i in 1:5000) {
    res[i] <- chisq.test(cbind(c(p1[i],1-p1[i]),c(p2[i],1-p2[i]))*k)$p.v
  }
  return(mean(res < alpha))
}
f1 <- Vectorize(foo, "k")

xx <- seq(50, 1000, 10)
n.grp <- xx/2

res1 <- f1(k = xx, pp1 = 0.25, pp2 = 0.15, alpha = 0.05)

plot(res1 ~ n.grp,
     type = "l",
     col = "red",
     xlab = "Number of Subjects Per Treatment Group",
     ylab = "Power",
     main = "Power Curves for a Variable with \n25% Events in Control vs. 15% in Active Groups",
     ylim = c(0, 1))
abline(h = 0.8,
       lty = 2)
nn <- n.grp[res1 == min(res1[res1 >= 0.8], na.rm = T)]
nn <- nn[!is.na(nn)]
abline(v = nn,
       lty = 3)
text(x = nn - 5,
     labels = paste("N =",
                    nn),
     y = 0.4,
     srt = 90)

res2 <- f1(k = xx, pp1 = 0.25, pp2 = 0.15, alpha = 0.01)

lines(res2 ~ n.grp,
      type = "l",
      col = "blue")
nn <- n.grp[res2 == min(res2[res2 >= 0.8], na.rm = T)]
nn <- nn[!is.na(nn)]
abline(v = nn,
       lty = 3)
text(x = nn - 5,
     labels = paste("N =",
                    nn),
     y = 0.4,
     srt = 90)

res3 <- f1(k = xx, pp1 = 0.25, pp2 = 0.15, alpha = 0.1)

lines(res3 ~ n.grp,
      type = "l",
      col = "green")
nn <- n.grp[res3 == min(res3[res3 >= 0.8], na.rm = T)]
nn <- nn[!is.na(nn)]
abline(v = nn,
       lty = 3)
text(x = nn - 5,
     labels = paste("N =",
                    nn),
     y = 0.4,
     srt = 90)

legend("bottomright",
       legend = paste("Alpha = ",
                      c(0.1, 0.05, 0.01)),
       col = c("green", "red", "blue"),
       lty = 1)

#****************************************************
# Nora's table and plot----

N1 <- 1000
N2 <- 1000
delta <- rep(c(50, 100, 150, 200), 4)
n1 <- rep(c(100, 200, 300, 400), each = 4)
n1
n2 <- n1 - delta
n2
n2[c(2:4, 8)] <- NA
n2

# Effect size
h <- ES.h(p1 = n1/N1,
          p2 = n2/N2)
h

xx <- seq(10, 500, by = 10)

foo <- Vectorize(pwr.2p.test, "h")
out <- foo(h = h,
           n = xx,
           sig.level = 0.05,
           alternative = "g")

for(i in 1:4) {
  plot(out[, i]$power ~ xx,
       type = "l",
       col = "red",
       xlab = "Number of Subjects Per Treatment Group",
       ylab = "Power",
       main = paste("Delta = ",
                    5*i,
                    "%",
                    sep = ""),
       ylim = c(0, 1))
  lines(out[, i + 4]$power ~ xx,
        type = "l",
        col = "green")
  lines(out[, i + 8]$power ~ xx,
        type = "l",
        col = "blue")
  lines(out[, i + 12]$power ~ xx,
        type = "l",
        col = "black")
  abline(h = 0.8,
         lty = 2)
  # nn <- xx[out$power == min(out$power[out$power >= 0.8])]
  # abline(v = nn,
  #        lty = 3)
  # text(x = nn - 5,
  #      labels = paste("N =",
  #                     nn),
  #      y = 0.4,
  #      srt = 90)
}

#****************************************************
# Plot----
pwrs <- list()
for(i in 1:16) {
  pwrs[[i]] <- out[, i]$power
}

pwrs <- do.call("data.frame", pwrs)
names(pwrs) <- paste("Ctrl",
                     n1/10,
                     "Delta",
                     delta/10)
pwrs <- data.table(x = xx,
                   pwrs)
ss <- lapply(pwrs[, -1, with = FALSE], function(a) {
  nn <- pwrs$x[a == min(a[a >= 0.8], na.rm = TRUE)]
  if(length(nn) == 1) {
    return(nn)
  } else {
    return(NA)
  }
})

d.l <- melt.data.table(pwrs, id.vars = "x")
d.l$ctrl <- paste(substr(as.character(d.l$variable), 6, 7),
                  "%", 
                  sep = "")
d.l$ctrl <- factor(d.l$ctrl, levels = unique(d.l$ctrl))
d.l$delta <- paste("Delta = ", 
                   substr(as.character(d.l$variable), 
                          14, 
                          nchar(as.character(d.l$variable))),
                   "%",
                   sep = "")
d.l$delta <- factor(d.l$delta, levels = unique(d.l$delta))
d.l

ggplot(data = d.l, 
       aes(x = x,
           y = value,
           group = ctrl,
           colour = ctrl)) +
  facet_wrap(~ delta,
             nrow = 2) +
  geom_line() + 
  scale_x_continuous("Number of Subjects per Group") + 
  scale_y_continuous("Power") +
  geom_hline(yintercept = 0.8,
             linetype="dashed") +
  guides(colour = guide_legend(title = "Control Group Risk",
                               title.position = "top",
                               nrow = 1)) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 0, hjust = 1))

# table
ss <- matrix(data = do.call("c", ss), nrow = 4, byrow = TRUE)
rownames(ss) <- unique(d.l$ctrl)
colnames(ss) <- unique(d.l$delta)
ss
# Project: JFK Stroke Recovery Program
# Description: data summary
# Author: Davit Sargsyan
# Date: 11/23/2016
#***************************************************************
require(data.table)
require(ggplot2)
require(lmerTest)

#***************************************************************
# Data----
dm <- fread("data/Patients to Export_Demographics 20161118.csv",
            sep = ",",
            header = TRUE)

dt.dm <- data.table(id = dm$`Medical Record No`,
                    bdt = as.Date(dm$`Date Of Birth`, "%m/%d/%Y"),
                    age = dm$Age,
                    race = dm$Race,
                    sex = factor(dm$Gender,
                                 levels = c("Female",
                                            "Male")),
                    hisp = (dm$`Hispanic Ethnicity` == "Yes"),
                    dead = dm$`Deceased_Y/N`,
                    deathdt = as.Date(dm$Deceased_Dt, "%m/%d/%Y"),
                    ins = factor(dm$`Health Insurance Name`))
dt.dm
summary(dt.dm)

mh <- fread("data/Patients to Export_Medical History 20161118.csv",
            sep = ",",
            header = TRUE)

dt.mh <- data.table(id = mh$`Medical Record No`,
                    stroke.admdt = as.Date(mh$`ACHosp DateOfAdmission`, "%m/%d/%Y"),
                    stroke.dschdt = as.Date(mh$`ACHosp DateOfDischarge`, "%m/%d/%Y"),
                    sev.type = factor(mh$`ACHosp Stroke Severity Type`),
                    sev.num = factor(mh$`ACHosp Stroke Severity Number`),
                    stroke.age = mh$`Age at Stroke`,
                    stroke.type = factor(mh$`Type of Stroke`),
                    himiparesis.left = (mh$Hemiparesis_Left == "Yes"),
                    himiparesis.right = (mh$Hemiparesis_Right == "Yes"),
                    himiparesis.bilat = (mh$Hemiparesis_Bilateral == "Yes"),
                    balance.ataxia = mh$`Balance/Ataxia` == "Yes",
                    speech = (mh$Speech == "Yes"),
                    disphagia = (mh$Dysphagia == "Yes"),
                    spasticity = (mh$Spasticity == "Yes"),
                    mh[, c(36:41, 43:65), with = FALSE])
summary(dt.mh)

dt1 <- merge(dt.dm, dt.mh, by = "id")

# Visits
vis <- fread("data/Patients to Export_Measurement 20161118.csv",
            sep = ",",
            header = TRUE)

vis[vis == 99999 | vis == 9999 | vis == 8888] <- NA
vis$`OT-AM-Daily Activity Score`[vis$`OT-AM-Daily Activity Score` > 125] <- NA

dt.vis <- data.table(id = vis$`Medical Record No`,
                     vdt = as.Date(vis$`Visit Date`, "%m/%d/%Y"),
                     dayid = as.numeric(as.character(vis$DaysId)),
                     grp = factor(vis$Group),
                     hgt = vis$Height,
                     wgt = vis$Weight,
                     bmi = vis$BMI,
                     sbp = vis$`SBP-HTN`,
                     dbp = vis$`DBP-HTN`,
                     ldl = factor(vis$`Fast/Nonfasting LDL`),
                     hba1c = vis$HbA1c,
                     phys.activ = factor(vis$`Physical Activity`),
                     nutr = factor(vis$Nutrition),
                     smoking = factor(vis$`Smoking Status`),
                     mob.score = vis$`PT-AM-PAC Basic Mobility Score`,
                     act.score = vis$`OT-AM-Daily Activity Score`,
                     cogn.score = vis$`ST-AM-Applied Cogn Score`)
setkey(dt.vis, id, vdt)
dt.vis[, days := as.numeric(as.character(difftime(time1 = vdt, 
                                                  time2 = min(vdt), 
                                                  units = "days"))),
       by = id]

# dt.vis[, grp.last.visit := grp[days == max(days, na.rm = TRUE)],
#        by = id]

dt.vis$ldl <- as.numeric(as.character(dt.vis$ldl))

summary(dt.vis)

#***************************************************************
# Visualization----
# Add death
death <- subset(dt1, 
                dead,
                select = c("id",
                           "deathdt"))
names(death)[2] <- "vdt"
death$dayid <- 11
death$mob.score <- death$act.score <- death$cogn.score <- 0
death$grp <- "Deceased"

# a. Mobility Scores----
tmp <- droplevels(subset(dt.vis, 
                         !is.na(mob.score)))
# Recalculate days from baseline
tmp[, days := as.numeric(as.character(difftime(time1 = vdt, 
                                               time2 = min(vdt), 
                                               units = "days"))),
    by = id]

# Last visit treatment assignment
tmp[, grp.last.visit := grp[days == max(days, na.rm = TRUE)][1],
    by = id]

tmp <- merge(death,
             droplevels(subset(tmp, 
                               grp.last.visit != "No")),
             by = c("id",
                    "vdt",
                    "dayid",
                    "grp",
                    "mob.score"),
             all = TRUE)
tmp$grp <- as.character(tmp$grp)
tmp$grp[tmp$grp == "No"] <- "Baseline"
tmp$grp <- factor(tmp$grp,
                  levels = c("Baseline",
                             "Study Group",
                             "Control Group",
                             "Deceased"))
setkey(tmp, id, dayid)

tmp$grp.last.visit <- as.character(tmp$grp.last.visit)
tmp$grp.last.visit[tmp$id %in% death$id] <- "Deceased"
tmp$grp.last.visit <- factor(tmp$grp.last.visit,
                             levels = c("Study Group",
                                        "Control Group",
                                        "Deceased"))

# Recalculate days from baseline
tmp[, days := as.numeric(as.character(difftime(time1 = vdt, 
                                               time2 = min(vdt), 
                                               units = "days"))),
    by = id]

# Plot
ggplot(tmp,
       aes(x = days,
           y = mob.score,
           colour = grp,
           group = id)) +
  facet_wrap(~ grp.last.visit,
             nrow = 1) +
  geom_line(position = position_dodge(0.3),
            size = 0.1,
            col = "black") +
  geom_point(position = position_dodge(0.3),
             size = 3,
             alpha = 0.5) +
  scale_x_continuous("Days from stroke") +
  scale_y_continuous("Scores") +
  ggtitle("Mobility Scores") +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = "Assigned Treatment Group at Each Visit",
                               title.position = "top")) +
  scale_color_manual(values = c("blue",
                                "green",
                                "red",
                                "black"))

# Model
m1 <- lmerTest::lmer(mob.score ~ grp.last.visit + (1|days) + (1|id),
           data = tmp)
m1
summary(m1)

# b. Activity Scores----
tmp <- droplevels(subset(dt.vis, 
                         !is.na(act.score)))
# Recalculate days from baseline
tmp[, days := as.numeric(as.character(difftime(time1 = vdt, 
                                               time2 = min(vdt), 
                                               units = "days"))),
    by = id]

# Last visit treatment assignment
tmp[, grp.last.visit := grp[days == max(days, na.rm = TRUE)][1],
    by = id]

tmp <- merge(death,
             droplevels(subset(tmp, 
                               grp.last.visit != "No")),
             by = c("id",
                    "vdt",
                    "dayid",
                    "grp",
                    "act.score"),
             all = TRUE)
tmp$grp <- as.character(tmp$grp)
tmp$grp[tmp$grp == "No"] <- "Baseline"
tmp$grp <- factor(tmp$grp,
                  levels = c("Baseline",
                             "Study Group",
                             "Control Group",
                             "Deceased"))
setkey(tmp, id, dayid)

tmp$grp.last.visit <- as.character(tmp$grp.last.visit)
tmp$grp.last.visit[tmp$id %in% death$id] <- "Deceased"
tmp$grp.last.visit <- factor(tmp$grp.last.visit,
                             levels = c("Study Group",
                                        "Control Group",
                                        "Deceased"))

# Recalculate days from baseline
tmp[, days := as.numeric(as.character(difftime(time1 = vdt, 
                                               time2 = min(vdt), 
                                               units = "days"))),
    by = id]

# Plot
ggplot(tmp,
       aes(x = days,
           y = act.score,
           colour = grp,
           group = id)) +
  facet_wrap(~ grp.last.visit,
             nrow = 1) +
  geom_line(position = position_dodge(0.3),
            size = 0.1,
            col = "black") +
  geom_point(position = position_dodge(0.3),
             size = 3,
             alpha = 0.5) +
  scale_x_continuous("Days from stroke") +
  scale_y_continuous("Scores") +
  ggtitle("Activity Scores") +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = "Assigned Treatment Group at Each Visit",
                               title.position = "top")) +
  scale_color_manual(values = c("blue",
                                "green",
                                "red",
                                "black"))

# Model
m2 <- lmerTest::lmer(act.score ~ grp.last.visit + (1|days) + (1|id),
                     data = tmp)
m2
summary(m2)


# c. Cognitive Scores----
tmp <- droplevels(subset(dt.vis, 
                         !is.na(cogn.score)))
# Recalculate days from baseline
tmp[, days := as.numeric(as.character(difftime(time1 = vdt, 
                                               time2 = min(vdt), 
                                               units = "days"))),
    by = id]

# Last visit treatment assignment
tmp[, grp.last.visit := grp[days == max(days, na.rm = TRUE)][1],
    by = id]

tmp <- merge(death,
             droplevels(subset(tmp, 
                               grp.last.visit != "No")),
             by = c("id",
                    "vdt",
                    "dayid",
                    "grp",
                    "cogn.score"),
             all = TRUE)
tmp$grp <- as.character(tmp$grp)
tmp$grp[tmp$grp == "No"] <- "Baseline"
tmp$grp <- factor(tmp$grp,
                  levels = c("Baseline",
                             "Study Group",
                             "Control Group",
                             "Deceased"))
setkey(tmp, id, dayid)

tmp$grp.last.visit <- as.character(tmp$grp.last.visit)
tmp$grp.last.visit[tmp$id %in% death$id] <- "Deceased"
tmp$grp.last.visit <- factor(tmp$grp.last.visit,
                             levels = c("Study Group",
                                        "Control Group",
                                        "Deceased"))

# Recalculate days from baseline
tmp[, days := as.numeric(as.character(difftime(time1 = vdt, 
                                               time2 = min(vdt), 
                                               units = "days"))),
    by = id]

# Plot
ggplot(tmp,
       aes(x = days,
           y = cogn.score,
           colour = grp,
           group = id)) +
  facet_wrap(~ grp.last.visit,
             nrow = 1) +
  geom_line(position = position_dodge(0.3),
            size = 0.1,
            col = "black") +
  geom_point(position = position_dodge(0.3),
             size = 3,
             alpha = 0.5) +
  scale_x_continuous("Days from stroke") +
  scale_y_continuous("Scores") +
  ggtitle("Cognition Scores") +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = "Assigned Treatment Group at Each Visit",
                               title.position = "top")) +
  scale_color_manual(values = c("blue",
                                "green",
                                "red",
                                "black"))

# Model
m3 <- lmerTest::lmer(cogn.score ~ grp.last.visit + (1|days) + (1|id),
                     data = tmp)
m3
summary(m3)

#***************************************************************
# Baseline----
# All baseline visits
dt.base <- droplevels(subset(dt.vis, grp == "No"))

# Baseline visits
dt.base <- droplevels(subset(dt.vis, grp == "No" & !is.na(hgt)))
length(unique(dt.base$id)) == nrow(dt.base)

# Tables
summary(dt.base)

# All follow-up visits----
table(dt.vis$dayid)
dt.fu <- droplevels(subset(dt.vis, dayid %in% 3:10))

# a. Mobility
dt.mob <- dcast.data.table(data = dt.fu,
                           id ~ dayid,
                           value.var = "mob.score")

write.csv(dt.mob, 
          file = "tmp/dt.mob.counts.csv",
          row.names = FALSE)

# b. Activity
dt.act <- dcast.data.table(data = dt.fu,
                           id ~ dayid,
                           value.var = "act.score")

write.csv(dt.act, 
          file = "tmp/dt.act.counts.csv",
          row.names = FALSE)

# c. Cognition
dt.cogn <- dcast.data.table(data = dt.fu,
                            id ~ dayid,
                            value.var = "cogn.score")

write.csv(dt.cogn, 
          file = "tmp/dt.cogn.counts.csv",
          row.names = FALSE)

# Analysis----
# Badsed on the above, remove subjects with multiple records for same visit
dt.fu <- droplevels(subset(dt.fu, !(id %in% c(322203,
                                              671955, 
                                              771438,
                                              855777,
                                              1125843,
                                              1150100,
                                              1274470,
                                              1277878,
                                              1278349))))

# Last visit treatment assignment
dt.fu[, grp.last.visit := grp[days == max(days, na.rm = TRUE)][1],
      by = id]

# a. Mobility
dt.mob <- dcast.data.table(data = dt.fu,
                           id + grp.last.visit ~ dayid,
                           value.var = "mob.score")

dt.mob <- merge(dt.mob,
               dt1[, c(1, 3:6), with = FALSE],
               by = "id")

tmp <- merge(dt.mob[, c("id",
                        "grp.last.visit"),
                    with = FALSE],
             dt.base[, c("id",
                         "mob.score"),
                     with = FALSE],
             by = "id")

dt.mob <- merge(tmp,
                dt.mob[, -2, with = FALSE],
                by = "id")

names(dt.mob)[3:10] <- paste("vis", 0:7, sep = "")

dt.mob <- merge(dt.mob,
                dt.base[, c("id",
                            "hgt",
                            "wgt",
                            "bmi",
                            "sbp",
                            "dbp",
                            "ldl",
                            "hba1c",
                            "smoking"),
                        with = FALSE],
                by = "id")
dt.mob

# Plot baseline
ggplot(data = dt.mob) +
  scale_x_discrete("Treatment Group") + 
  scale_y_continuous("Score") + 
  ggtitle("Mobility at Baseline (V0)") +
  geom_boxplot(aes(x = grp.last.visit,
                   y = vis0,
                   outlier.shape = NA)) +
  geom_point(aes(x = grp.last.visit,
                 y = vis0,
                 group = id,
                 colour = bmi),
             size = 3,
             alpha = 0.6,
             position = position_dodge(0.3))

summary(lm(vis0 ~ grp.last.visit + age + sex + hisp + bmi + sbp + dbp + ldl + hba1c + smoking,
           data = dt.mob))

# Delta V1 - V0----
dt.mob$d.v1.v0 <- dt.mob$vis1 - dt.mob$vis0
dt.v1.v0 <- subset(dt.mob, !is.na(d.v1.v0))

# Delta V1 - V0
ggplot(data = dt.v1.v0) +
  scale_x_discrete("Treatment Group") + 
  scale_y_continuous("Score") + 
  ggtitle("Delta Mobility (V1 - V0)") +
  geom_boxplot(aes(x = grp.last.visit,
                   y = d.v1.v0,
                   outlier.shape = NA)) +
  geom_point(aes(x = grp.last.visit,
                 y = d.v1.v0,
                 group = id,
                 colour = bmi),
             size = 3,
             alpha = 0.6,
             position = position_dodge(0.3))

summary(lm(d.v1.v0 ~ grp.last.visit + age + sex + hisp + vis0 + bmi + sbp + dbp + ldl + hba1c + smoking,
           data = dt.v1.v0))

# Delta V2 - V0----
dt.mob$d.v2.v0 <- dt.mob$vis2 - dt.mob$vis0
dt.v2.v0 <- subset(dt.mob, !is.na(d.v2.v0))

# Plot deltas
ggplot(data = dt.v2.v0) +
  scale_x_discrete("Treatment Group") + 
  scale_y_continuous("Score") + 
  ggtitle("Delta Mobility (V2 - V0)") +
  geom_boxplot(aes(x = grp.last.visit,
                   y = d.v2.v0,
                   outlier.shape = NA)) +
  geom_point(aes(x = grp.last.visit,
                 y = d.v2.v0,
                 group = id,
                 colour = bmi),
             size = 3,
             alpha = 0.6,
             position = position_dodge(0.3))

summary(lm(d.v2.v0 ~ grp.last.visit + age + sex + hisp + vis0 + bmi + sbp + dbp + ldl + hba1c + smoking,
           data = dt.v2.v0))

# Delta V3 - V0----
dt.mob$d.v3.v0 <- dt.mob$vis3 - dt.mob$vis0
dt.v3.v0 <- subset(dt.mob, !is.na(d.v3.v0))

# Plot deltas
ggplot(data = dt.v3.v0) +
  scale_x_discrete("Treatment Group") + 
  scale_y_continuous("Score") + 
  ggtitle("Delta Mobility (V3 - V0)") +
  geom_boxplot(aes(x = grp.last.visit,
                   y = d.v3.v0,
                   outlier.shape = NA)) +
  geom_point(aes(x = grp.last.visit,
                 y = d.v3.v0,
                 group = id,
                 colour = bmi),
             size = 3,
             alpha = 0.6,
             position = position_dodge(0.3))

summary(lm(d.v3.v0 ~ grp.last.visit + age + sex + hisp + vis0 + bmi + sbp + dbp + ldl + hba1c + smoking,
           data = dt.v3.v0))
# Project: JFK Stroke Recovery Program
# Description: data summary
# Author: Davit Sargsyan
# Created: 11/23/2016
# Last Modified: 02/17/2017
#***************************************************************
require(data.table)
require(ggplot2)
require(lmerTest)

#***************************************************************
# Data----
# 11/23/2016
# dm <- fread("data/Patients to Export_Demographics 20161118.csv",
#             sep = ",",
#             header = TRUE)
# 02/04/2017
dm <- fread("data/Qry_Patients to Export_Demographics -2017Feb3 by CatNo.csv",
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

# 11/23/2016
# mh <- fread("data/Patients to Export_Medical History 20161118.csv",
#             sep = ",",
#             header = TRUE)
# 02/04/2017
mh <- fread("data/Qry_Patients to Export_Medical History - 2017Feb3 by CatNo.csv",
            sep = ",",
            header = TRUE)

dt.mh <- data.table(id = mh$`Medical Record No`,
                    strokedt = as.Date(mh$`Date of Stroke`, "%m/%d/%Y"),
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
# 11/23/2016
# vis <- fread("data/Patients to Export_Measurement 20161118.csv",
#             sep = ",",
#             header = TRUE)
# 02/04/2017
vis <- fread("data/Qry_Patients to Export_Measurement - 2017Feb3 by MedRec#.csv",
             sep = ",",
             header = TRUE)

vis[vis == 99999 | vis == 9999 | vis == 8888] <- NA
vis$`OT-AM-Daily Activity Score`[vis$`OT-AM-Daily Activity Score` > 125] <- NA

dt.vis <- data.table(id = vis$`Medical Record No`,
                     vdt = as.Date(vis$`Visit Date`, "%m/%d/%Y"),
                     days = as.numeric(as.character(vis$Days_afterOnset)),
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
dt.vis$ldl <- as.numeric(as.character(dt.vis$ldl))

setkey(dt.vis, id, vdt)
summary(dt.vis)

#***************************************************************
# Visualization----
# Death----
death <- subset(dt1, 
                dead,
                select = c("id",
                           "strokedt",
                           "deathdt"))
names(death)[2] <- "vdt"
death$dayid <- 11
death$mob.score <- death$act.score <- death$cogn.score <- 0
death$grp <- "Deceased"
death[, days := as.numeric(as.character(difftime(time1 = deathdt, 
                                                 time2 = vdt, 
                                                 units = "days")))]
death

# a. Mobility Scores----
tmp <- droplevels(subset(dt.vis,
                         !is.na(mob.score)))

tmp <- merge(death[, -c("act.score", "cogn.score"), with = FALSE],
             tmp,
             by = c("id",
                    "vdt",
                    "days",
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

# Last visit treatment assignment
tmp[, grp.last.visit := grp[days == max(days, na.rm = TRUE)][length(max(days, na.rm = TRUE))],
    by = id]

tmp$grp.last.visit <- as.character(tmp$grp.last.visit)
tmp$grp.last.visit[tmp$id %in% death$id] <- "Deceased"
tmp$grp.last.visit <- factor(tmp$grp.last.visit,
                             levels = c("Study Group",
                                        "Control Group",
                                        "Deceased",
                                        "Baseline"))

# Print deceased patients' data
tmp[grp.last.visit == "Deceased", ]

# Plot
png(filename = "tmp/mobility.png",
     height = 7,
     width = 7,
     units = 'in',
     res = 300)
ggplot(tmp,
       aes(x = days,
           y = mob.score,
           colour = grp,
           group = id)) +
  facet_wrap(~ grp.last.visit,
             nrow = 2) +
  geom_line(position = position_dodge(0.3),
            size = 0.1,
            col = "black") +
  geom_point(position = position_dodge(0.3),
             size = 3,
             alpha = 0.5) +
  scale_x_continuous("Days from stroke") +
  scale_y_continuous("Scores") +
  ggtitle("Mobility Score By Last Record Assignment Group") +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = "Assigned Treatment Group at Each Visit",
                               title.position = "top")) +
  scale_color_manual(values = c("blue",
                                "green",
                                "red",
                                "black"))
graphics.off()

# Model
m1 <- lmerTest::lmer(mob.score ~ grp.last.visit + (1|days) + (1|id),
           data = tmp)
m1
summary(m1)

# b. Activity Scores----
tmp <- droplevels(subset(dt.vis,
                         !is.na(act.score)))

tmp <- merge(death[, -c("mob.score", "cogn.score"), with = FALSE],
             tmp,
             by = c("id",
                    "vdt",
                    "days",
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

# Last visit treatment assignment
tmp[, grp.last.visit := grp[days == max(days, na.rm = TRUE)][length(max(days, na.rm = TRUE))],
    by = id]

tmp$grp.last.visit <- as.character(tmp$grp.last.visit)
tmp$grp.last.visit[tmp$id %in% death$id] <- "Deceased"
tmp$grp.last.visit <- factor(tmp$grp.last.visit,
                             levels = c("Study Group",
                                        "Control Group",
                                        "Deceased",
                                        "Baseline"))

# Plot
png(filename = "tmp/activity.png",
    height = 7,
    width = 7,
    units = 'in',
    res = 300)
ggplot(tmp,
       aes(x = days,
           y = act.score,
           colour = grp,
           group = id)) +
  facet_wrap(~ grp.last.visit,
             nrow = 2) +
  geom_line(position = position_dodge(0.3),
            size = 0.1,
            col = "black") +
  geom_point(position = position_dodge(0.3),
             size = 3,
             alpha = 0.5) +
  scale_x_continuous("Days from stroke") +
  scale_y_continuous("Scores") +
  ggtitle("Activity Score By Last Record Assignment Group") +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = "Assigned Treatment Group at Each Visit",
                               title.position = "top")) +
  scale_color_manual(values = c("blue",
                                "green",
                                "red",
                                "black"))
graphics.off()

# Model
m2 <- lmerTest::lmer(act.score ~ grp.last.visit + (1|days) + (1|id),
                     data = tmp)
m2
summary(m2)

# c. Cognitive Scores----
tmp <- droplevels(subset(dt.vis,
                         !is.na(cogn.score)))

tmp <- merge(death[, -c("mob.score", "act.score"), with = FALSE],
             tmp,
             by = c("id",
                    "vdt",
                    "days",
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

# Last visit treatment assignment
tmp[, grp.last.visit := grp[days == max(days, na.rm = TRUE)][length(max(days, na.rm = TRUE))],
    by = id]

tmp$grp.last.visit <- as.character(tmp$grp.last.visit)
tmp$grp.last.visit[tmp$id %in% death$id] <- "Deceased"
tmp$grp.last.visit <- factor(tmp$grp.last.visit,
                             levels = c("Study Group",
                                        "Control Group",
                                        "Deceased",
                                        "Baseline"))

# Plot
png(filename = "tmp/cognition.png",
    height = 7,
    width = 7,
    units = 'in',
    res = 300)
ggplot(tmp,
       aes(x = days,
           y = cogn.score,
           colour = grp,
           group = id)) +
  facet_wrap(~ grp.last.visit,
             nrow = 2) +
  geom_line(position = position_dodge(0.3),
            size = 0.1,
            col = "black") +
  geom_point(position = position_dodge(0.3),
             size = 3,
             alpha = 0.5) +
  scale_x_continuous("Days from stroke") +
  scale_y_continuous("Scores") +
  ggtitle("Cognition Score By Last Record Assignment Group") +
  theme(legend.position = "top") +
  guides(colour = guide_legend(title = "Assigned Treatment Group at Each Visit",
                               title.position = "top")) +
  scale_color_manual(values = c("blue",
                                "green",
                                "red",
                                "black"))
graphics.off()

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
CONTINUE HERE!!!
  
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

# a. Mobility Analysis----
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

# b. Activity Analysis----
dt.act <- dcast.data.table(data = dt.fu,
                           id + grp.last.visit ~ dayid,
                           value.var = "act.score")

dt.act <- merge(dt.act,
                dt1[, c(1, 3:6), with = FALSE],
                by = "id")

tmp <- merge(dt.act[, c("id",
                        "grp.last.visit"),
                    with = FALSE],
             dt.base[, c("id",
                         "act.score"),
                     with = FALSE],
             by = "id")

dt.act <- merge(tmp,
                dt.act[, -2, with = FALSE],
                by = "id")

names(dt.act)[3:10] <- paste("vis", 0:7, sep = "")

dt.act <- merge(dt.act,
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
dt.act

# Plot baseline
ggplot(data = dt.act) +
  scale_x_discrete("Treatment Group") + 
  scale_y_continuous("Score") + 
  ggtitle("Activity at Baseline (V0)") +
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
           data = dt.act))

# Delta V1 - V0----
dt.act$d.v1.v0 <- dt.act$vis1 - dt.act$vis0
dt.v1.v0 <- subset(dt.act, !is.na(d.v1.v0))

# Delta V1 - V0
ggplot(data = dt.v1.v0) +
  scale_x_discrete("Treatment Group") + 
  scale_y_continuous("Score") + 
  ggtitle("Delta actility (V1 - V0)") +
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
dt.act$d.v2.v0 <- dt.act$vis2 - dt.act$vis0
dt.v2.v0 <- subset(dt.act, !is.na(d.v2.v0))

# Plot deltas
ggplot(data = dt.v2.v0) +
  scale_x_discrete("Treatment Group") + 
  scale_y_continuous("Score") + 
  ggtitle("Delta actility (V2 - V0)") +
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
dt.act$d.v3.v0 <- dt.act$vis3 - dt.act$vis0
dt.v3.v0 <- subset(dt.act, !is.na(d.v3.v0))

# Plot deltas
ggplot(data = dt.v3.v0) +
  scale_x_discrete("Treatment Group") + 
  scale_y_continuous("Score") + 
  ggtitle("Delta actility (V3 - V0)") +
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

#*************************************************************
# Tables----
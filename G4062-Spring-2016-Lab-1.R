## Example Code ##

gss = read.csv(file.choose()) ## choose the GSS Networks csv.file from Courseworks ##

gss$rlife = 4-gss$life
table(gss$rlife)

educs = c("educ1", "educ2", "educ3", "educ4", "educ5")
gss$aveeduc=rowMeans(gss[, educs ], na.rm=TRUE)
summary(gss$aveeduc)

reg1 <- lm(rlife ~ aveeduc, data=gss)
plot(gss$aveeduc, gss$rlife)
abline(reg1)

summary(lm(rlife ~ aveeduc + numgiven, data=gss))

summary(lm(rlife ~ aveeduc + educ + numgiven, data=gss))

summary(lm(rlife ~ aveeduc + aveeduc + numgiven + educ + age + attend + as.factor(marital) + as.factor(race) + childs + sex , data=gss))

summary(lm(rlife ~ aveeduc + aveeduc*educ + numgiven  + age + attend + as.factor(marital) + as.factor(race) + childs + sex , data=gss))

educs = c("educ1", "educ2", "educ3", "educ4", "educ5")
sub <- gss[, educs]
sub2=transform(sub, SD=apply(sub,1, sd, na.rm = TRUE))
colnames(sub2)[6] <- "sdeduc"
gss <- data.frame(gss, sub2[,"sdeduc"])
colnames(gss)
colnames(gss)[2112] <- "sdeduc"

lm1 = lm(rlife ~ aveeduc + aveeduc + educ + numgiven  + sdeduc + age + attend + as.factor(marital) + as.factor(race) + childs + sex , data=gss)
summary(lm1)


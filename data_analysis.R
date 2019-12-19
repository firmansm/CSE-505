ds <- read.csv(file.choose())
ds_ana <- ds
str(ds_ana)
names(ds_ana)

ds_ana[c("q1","q2", "q3", "q4", "q5", "q6")] <- NA

for (i in (1:length(ds_ana$q1))){
  if (isTRUE(ds_ana[i,13]=="Unknown")) {
    ds_ana$q1[i] <- 1
  }
  else {
    ds_ana$q1[i] <- 0
  }
}

for (i in (1:length(ds_ana$q2))){
  if (isTRUE(ds_ana[i,14]=="Unknown")) {
    ds_ana$q2[i] <- 1
  }
  else {
    ds_ana$q2[i] <- 0
  }
}

for (i in (1:length(ds_ana$q3))){
  if (isTRUE(ds_ana[i,16]=="Unknown")) {
    ds_ana$q3[i] <- 1
  }
  else {
    ds_ana$q3[i] <- 0
  }
}

for (i in (1:length(ds_ana$q4))){
  if (isTRUE(ds_ana[i,17]=="Unknown")) {
    ds_ana$q4[i] <- 1
  }
  else {
    ds_ana$q4[i] <- 0
  }
}

for (i in (1:length(ds_ana$q5))){
  if (isTRUE(ds_ana[i,15]=="Unknown  ")) {
    ds_ana$q5[i] <- 1
  }
  else {
    ds_ana$q5[i] <- 0
  }
}

for (i in (1:length(ds_ana$q6))){
  if (isTRUE(ds_ana[i,18]=="Unknown")) {
    ds_ana$q6[i] <- 1
  }
  else {
    ds_ana$q6[i] <- 0
  }
}

prop.test(sum(ds_ana$q1), length(ds_ana$q1), correct=FALSE)
prop.test(sum(ds_ana$q2), length(ds_ana$q2), correct=FALSE)
prop.test(sum(ds_ana$q3), length(ds_ana$q3), correct=FALSE)
prop.test(sum(ds_ana$q4), length(ds_ana$q4), correct=FALSE)
prop.test(sum(ds_ana$q5), length(ds_ana$q5), correct=FALSE)
prop.test(sum(ds_ana$q6), length(ds_ana$q6), correct=FALSE)

chisq.test(table(ds_ana$What.causes.storm.))
chisq.test(table(ds_ana$What.causes.forest.fire.))
chisq.test(table(ds_ana$Why.Mary.study.))
chisq.test(table(ds_ana$Why.Elizabeth.ask.))
chisq.test(table(ds_ana$What.are.present.))
chisq.test(table(ds_ana$Who.are.in.the.living.room.))
chisq.test(prop.test(table(ds_ana$What.causes.storm.))
           
ds_ana$context <- rowMeans(ds_ana[,19:22])
ds_ana$joint <- rowMeans(ds_ana[,23:24])
ds_ana$all <- rowMeans(ds_ana[,19:24])
           
mod1 <- lm(ds_ana$all ~ ds_ana$Duration..in.seconds.+ds_ana$Gender+ds_ana$Discpline+ds_ana$Logic.Course+ds_ana$Education)
summary(mod1)
mod1$coefficients
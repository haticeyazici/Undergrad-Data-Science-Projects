data = read.csv("/Users/haticeyazici/Desktop/Senior👩🏻‍🎓/Summer-2024/MATH 4322/final_project/data.csv")
glm.data = glm(output ~ age + sex + cp + trtbps + chol + fbs + restecg + thalachh + 
                 exng + oldpeak + slp + caa + thall, data=data, family = 'binomial')

summary(glm.data)
step(glm.data)

cor(data)


glm.data2 = glm(output ~ sex + cp + trtbps + restecg + thalachh + 
                  exng + oldpeak + slp + caa + thall, data=data, family = 'binomial')
summary(glm.data2)

step(glm.data2)

summary(data)

as.factor(data$fbs)


glm.data3 = glm(output ~ sex + cp + trtbps + thalachh + 
                  exng + oldpeak + slp + caa + thall, data=data, family = 'binomial')
summary(glm.data3)


fit.prec = predict(glm.data2, type="response")
fit.pred = ifelse(fit.prec < 0.5, 0, 1)
conf.table = table(fit.pred, data$output)

set.seed(100)
sample1 = sample(1:nrow(data),floor(nrow(data)*.8))
train1.data = data[sample1,]
test1.data = data[-sample1,]


#use K = 10
library(boot)
cv.error.10 = rep(0,5)
set.seed(100)
for (i in 1:5) {
  glm.fit = glm.data2
  cv.error.10[i] = cv.glm(data,glm.fit,K = 10)$delta[1]
}

glm.data.train = glm(output ~ sex + cp + trtbps + thalachh + 
                       exng + oldpeak + slp + caa + thall, data=train1.data, family = 'binomial')

fit.prec.test = predict(glm.data.train, type="response",newdata=test1.data)
fit.pred.test = ifelse(fit.prec.test < 0.5, 0, 1)
conf.table.test = table(fit.pred.test, test1.data$output)













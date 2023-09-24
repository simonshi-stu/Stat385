(a)dim(data1)

parta = lm(api00 ~ meals, data = data1)
summary(parta)

confint(parta, level = 0.9)

difference = data.frame(meals = 1)
predict(parta, newdata = difference, interval = 'confidence', level = 0.9)

(b)
new_data = data.frame(meals = 70)
predict(parta, newdata = new_data, interval = 'confidence', level = 0.95)

(c)
new_data2 = data.frame(meals = 70)
predict(parta, newdata = new_data2, interval = 'confidence', level = 0.9)

(d)
a = predict(parta, newdata = new_data, interval = 'confidence', level = 0.95)
b = predict(parta, newdata = new_data2, interval = 'confidence', level = 0.9)
pred = rbind(a, b)
pred

(e)
anova(parta)

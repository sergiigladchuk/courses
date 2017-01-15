w.vector <- seq(0.9,1.1,by=0.01)
source('~/Documents/courses/modelling/exam/q4_d_function.R')
fixationN20 <- rep(1,length(w.vector));
fixationN200 <- rep(1,length(w.vector));
for (i in 1:length(w.vector)){
  fixationN20[i] <- q4_d_function(20,w.vector[i]);
  fixationN200[i] <- q4_d_function(200,w.vector[i]);
}

#ploting
plot(fixationN20~w.vector, type='l', xlab='fitness value, w', ylab='fixation probability',
     ylim=c(0,max(fixationN20,fixationN200)), col = 'red')
lines(fixationN200~w.vector, col='blue')
legend('topleft',legend=c('Population 20','Population 200'), 
       col = c('red','blue'), lty=c(1,1))
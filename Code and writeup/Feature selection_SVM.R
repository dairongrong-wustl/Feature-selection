library(e1071)
tune.linear = tune(svm, 
                Classes~.,
                data = training_data_complete, 
                kernel = "linear", 
                ranges=list(cost=c(0.1,1,10,100)))
tune.Homogeneous = tune(svm, 
                   Classes~.,
                   data = training_data_complete, 
                   kernel = "polynomial", 
                   degree = 2,
                   coef0 =0,
                   ranges=list(cost=c(0.1,1,10,100)))
tune.Inhomogeneous = tune(svm, 
                        Classes~.,
                        data = training_data_complete, 
                        kernel = "polynomial", 
                        degree = 2,
                        coef0 =1,
                        ranges=list(cost=c(0.1,1,10,100)))
Homogenous_predict = predict(tune.Homogeneous$best.model, newdata = training_data_complete)
table(training_data_complete$Classes,Homogenous_predict)

w_linear = t(tune.linear$best.model$coefs) %*% tune.linear$best.model$SV

write.table(w_linear, "w_linear.txt", row.names = FALSE, col.names = FALSE)

w_Homo = t(tune.Homogeneous$best.model$coefs) %*% tune.Homogeneous$best.model$SV

write.table(w_Homo, "w_Homo.txt", row.names = FALSE, col.names = FALSE)

w_inHomo = t(tune.Inhomogeneous$best.model$coefs) %*% tune.Inhomogeneous$best.model$SV

write.table(w_inHomo, "w_inHomo.txt", row.names = FALSE, col.names = FALSE)


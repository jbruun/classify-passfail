ROC_PS_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_qda[[i]]<-ROC
}

ROC_CD_qda<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD,p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_qda[[i]]<-ROC
}

ROC_ICS_qda<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS,p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_qda[[i]]<-ROC
}

ROC_PS_justpass_qda<-list()
for(i in 1:100){
  predPS_x<-jackPredQDA(centPS,outcome = "justpass",p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_qda[[i]]<-ROC
}
##PROBLEM: RANK DEFICIENCY??###
ROC_CD_justpass_qda<-list()
for(i in 1:100){
  predCD_x<-jackPredQDA(centCD, outcome = "justpass",p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_qda[[i]]<-ROC
}
### PROBLEMS OVER###

ROC_ICS_justpass_qda<-list()
for(i in 1:100){
  predICS_x<-jackPredQDA(centICS, outcome = "justpass",p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_qda[[i]]<-ROC
}


save(ROC_PS_qda,ROC_CD_qda,ROC_ICS_qda, ROC_PS_justpass_qda,ROC_CD_justpass_qda,ROC_ICS_justpass_qda,file="data/ROC_qdareg.Rdata")

ROC_PS_TPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_FPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_PPV_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_SR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_PS_TPR_qda[i,1:7]<-ROC_PS_qda[[i]]$TPR[1:7]
  ROC_PS_FPR_qda[i,1:7]<-ROC_PS_qda[[i]]$FPR[1:7]
  ROC_PS_PPV_qda[i,1:7]<-ROC_PS_qda[[i]]$PPV[1:7]
  ROC_PS_SR_qda[i,1:7]<-ROC_PS_qda[[i]]$SR[1:7]
}

ROC_PS_TPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_FPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_PPV_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_SR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_PS_TPR_qda[i,1:7]<-ROC_PS_qda[[i]]$TPR[1:7]
  ROC_PS_FPR_qda[i,1:7]<-ROC_PS_qda[[i]]$FPR[1:7]
  ROC_PS_PPV_qda[i,1:7]<-ROC_PS_qda[[i]]$PPV[1:7]
  ROC_PS_SR_qda[i,1:7]<-ROC_PS_qda[[i]]$SR[1:7]
}

ROC_CD_TPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_FPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_PPV_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_SR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_CD_TPR_qda[i,1:7]<-ROC_CD_qda[[i]]$TPR[1:7]
  ROC_CD_FPR_qda[i,1:7]<-ROC_CD_qda[[i]]$FPR[1:7]
  ROC_CD_PPV_qda[i,1:7]<-ROC_CD_qda[[i]]$PPV[1:7]
  ROC_CD_SR_qda[i,1:7]<-ROC_CD_qda[[i]]$SR[1:7]
}

ROC_ICS_TPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_FPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_PPV_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_SR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_ICS_TPR_qda[i,1:7]<-ROC_ICS_qda[[i]]$TPR[1:7]
  ROC_ICS_FPR_qda[i,1:7]<-ROC_ICS_qda[[i]]$FPR[1:7]
  ROC_ICS_PPV_qda[i,1:7]<-ROC_ICS_qda[[i]]$PPV[1:7]
  ROC_ICS_SR_qda[i,1:7]<-ROC_ICS_qda[[i]]$SR[1:7]
}

ROC_PS_justpass_TPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_FPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_PPV_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_SR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_PS_justpass_TPR_qda[i,1:7]<-ROC_PS_justpass_qda[[i]]$TPR[1:7]
  ROC_PS_justpass_FPR_qda[i,1:7]<-ROC_PS_justpass_qda[[i]]$FPR[1:7]
  ROC_PS_justpass_PPV_qda[i,1:7]<-ROC_PS_justpass_qda[[i]]$PPV[1:7]
  ROC_PS_justpass_SR_qda[i,1:7]<-ROC_PS_justpass_qda[[i]]$SR[1:7]
}

ROC_CD_justpass_TPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_FPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_PPV_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_SR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_CD_justpass_TPR_qda[i,1:7]<-ROC_CD_justpass_qda[[i]]$TPR[1:7]
  ROC_CD_justpass_FPR_qda[i,1:7]<-ROC_CD_justpass_qda[[i]]$FPR[1:7]
  ROC_CD_justpass_PPV_qda[i,1:7]<-ROC_CD_justpass_qda[[i]]$PPV[1:7]
  ROC_CD_justpass_SR_qda[i,1:7]<-ROC_CD_justpass_qda[[i]]$SR[1:7]
}

ROC_ICS_justpass_TPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_FPR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_PPV_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_SR_qda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_ICS_justpass_TPR_qda[i,1:7]<-ROC_ICS_justpass_qda[[i]]$TPR[1:7]
  ROC_ICS_justpass_FPR_qda[i,1:7]<-ROC_ICS_justpass_qda[[i]]$FPR[1:7]
  ROC_ICS_justpass_PPV_qda[i,1:7]<-ROC_ICS_justpass_qda[[i]]$PPV[1:7]
  ROC_ICS_justpass_SR_qda[i,1:7]<-ROC_ICS_justpass_qda[[i]]$SR[1:7]
}

PS_AUC_qda<-vector()
for (i in 1:7){
  PS_AUC_qda[i]<-simple_auc(ROC_PS_TPR_qda[,i],ROC_PS_FPR_qda[,i])
}

CD_AUC_qda<-vector()
for (i in 1:7){
  CD_AUC_qda[i]<-simple_auc(ROC_CD_TPR_qda[,i],ROC_CD_FPR_qda[,i])
}

ICS_AUC_qda<-vector()
for (i in 1:7){
  ICS_AUC_qda[i]<-simple_auc(ROC_ICS_TPR_qda[,i],ROC_ICS_FPR_qda[,i])
}

PS_AUC_JP_qda<-vector()
for (i in 1:7){
  PS_AUC_JP_qda[i]<-simple_auc(ROC_PS_justpass_TPR_qda[,i],ROC_PS_justpass_FPR_qda[,i])
}

CD_AUC_JP_qda<-vector()
for (i in 1:7){
  CD_AUC_JP_qda[i]<-simple_auc(ROC_CD_justpass_TPR_qda[,i],ROC_CD_justpass_FPR_qda[,i])
}

ICS_AUC_JP_qda<-vector()
for (i in 1:7){
  ICS_AUC_JP_qda[i]<-simple_auc(ROC_ICS_justpass_TPR_qda[,i],ROC_ICS_justpass_FPR_qda[,i])
}
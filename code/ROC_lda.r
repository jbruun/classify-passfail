ROC_PS_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_lda[[i]]<-ROC
}

ROC_CD_lda<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD,p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_lda[[i]]<-ROC
}

ROC_ICS_lda<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS,p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_lda[[i]]<-ROC
}

ROC_PS_justpass_lda<-list()
for(i in 1:100){
  predPS_x<-jackPredLDA(centPS,outcome = "justpass",p=i/100)
  ROC<-ROCplusWeeks(predPS_x)
  ROC_PS_justpass_lda[[i]]<-ROC
}

ROC_CD_justpass_lda<-list()
for(i in 1:100){
  predCD_x<-jackPredLDA(centCD, outcome = "justpass",p=i/100)
  ROC<-ROCplusWeeks(predCD_x)
  ROC_CD_justpass_lda[[i]]<-ROC
}

ROC_ICS_justpass_lda<-list()
for(i in 1:100){
  predICS_x<-jackPredLDA(centICS, outcome = "justpass",p=i/100)
  ROC<-ROCplusWeeks(predICS_x)
  ROC_ICS_justpass_lda[[i]]<-ROC
}


save(ROC_PS_lda,ROC_CD_lda,ROC_ICS_lda, ROC_PS_justpass_lda,ROC_CD_justpass_lda,ROC_ICS_justpass_lda,file="data/ROC_ldareg.Rdata")

ROC_PS_TPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_FPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_PPV_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_SR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_PS_TPR_lda[i,1:7]<-ROC_PS_lda[[i]]$TPR[1:7]
  ROC_PS_FPR_lda[i,1:7]<-ROC_PS_lda[[i]]$FPR[1:7]
  ROC_PS_PPV_lda[i,1:7]<-ROC_PS_lda[[i]]$PPV[1:7]
  ROC_PS_SR_lda[i,1:7]<-ROC_PS_lda[[i]]$SR[1:7]
}

ROC_CD_TPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_FPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_PPV_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_SR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_CD_TPR_lda[i,1:7]<-ROC_CD_lda[[i]]$TPR[1:7]
  ROC_CD_FPR_lda[i,1:7]<-ROC_CD_lda[[i]]$FPR[1:7]
  ROC_CD_PPV_lda[i,1:7]<-ROC_CD_lda[[i]]$PPV[1:7]
  ROC_CD_SR_lda[i,1:7]<-ROC_CD_lda[[i]]$SR[1:7]
}

ROC_ICS_TPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_FPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_PPV_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_SR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_ICS_TPR_lda[i,1:7]<-ROC_ICS_lda[[i]]$TPR[1:7]
  ROC_ICS_FPR_lda[i,1:7]<-ROC_ICS_lda[[i]]$FPR[1:7]
  ROC_ICS_PPV_lda[i,1:7]<-ROC_ICS_lda[[i]]$PPV[1:7]
  ROC_ICS_SR_lda[i,1:7]<-ROC_ICS_lda[[i]]$SR[1:7]
}

ROC_PS_justpass_TPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_FPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_PPV_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_PS_justpass_SR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_PS_justpass_TPR_lda[i,1:7]<-ROC_PS_justpass_lda[[i]]$TPR[1:7]
  ROC_PS_justpass_FPR_lda[i,1:7]<-ROC_PS_justpass_lda[[i]]$FPR[1:7]
  ROC_PS_justpass_PPV_lda[i,1:7]<-ROC_PS_justpass_lda[[i]]$PPV[1:7]
  ROC_PS_justpass_SR_lda[i,1:7]<-ROC_PS_justpass_lda[[i]]$SR[1:7]
}

ROC_CD_justpass_TPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_FPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_PPV_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_CD_justpass_SR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_CD_justpass_TPR_lda[i,1:7]<-ROC_CD_justpass_lda[[i]]$TPR[1:7]
  ROC_CD_justpass_FPR_lda[i,1:7]<-ROC_CD_justpass_lda[[i]]$FPR[1:7]
  ROC_CD_justpass_PPV_lda[i,1:7]<-ROC_CD_justpass_lda[[i]]$PPV[1:7]
  ROC_CD_justpass_SR_lda[i,1:7]<-ROC_CD_justpass_lda[[i]]$SR[1:7]
}

ROC_ICS_justpass_TPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_FPR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_PPV_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
ROC_ICS_justpass_SR_lda<-data.frame(w1=double(),w2=double(),w3=double(),w4=double(),w5=double(),w6=double(),w7=double())
for (i in 1:100){
  ROC_ICS_justpass_TPR_lda[i,1:7]<-ROC_ICS_justpass_lda[[i]]$TPR[1:7]
  ROC_ICS_justpass_FPR_lda[i,1:7]<-ROC_ICS_justpass_lda[[i]]$FPR[1:7]
  ROC_ICS_justpass_PPV_lda[i,1:7]<-ROC_ICS_justpass_lda[[i]]$PPV[1:7]
  ROC_ICS_justpass_SR_lda[i,1:7]<-ROC_ICS_justpass_lda[[i]]$SR[1:7]
}

PS_AUC_lda<-vector()
for (i in 1:7){
  PS_AUC_lda[i]<-simple_auc(ROC_PS_TPR_lda[,i],ROC_PS_FPR_lda[,i])
}

CD_AUC_lda<-vector()
for (i in 1:7){
  CD_AUC_lda[i]<-simple_auc(ROC_CD_TPR_lda[,i],ROC_CD_FPR_lda[,i])
}

ICS_AUC_lda<-vector()
for (i in 1:7){
  ICS_AUC_lda[i]<-simple_auc(ROC_ICS_TPR_lda[,i],ROC_ICS_FPR_lda[,i])
}

PS_AUC_JP_lda<-vector()
for (i in 1:7){
  PS_AUC_JP_lda[i]<-simple_auc(ROC_PS_justpass_TPR_lda[,i],ROC_PS_justpass_FPR_lda[,i])
}

CD_AUC_JP_lda<-vector()
for (i in 1:7){
  CD_AUC_JP_lda[i]<-simple_auc(ROC_CD_justpass_TPR_lda[,i],ROC_CD_justpass_FPR_lda[,i])
}

ICS_AUC_JP_lda<-vector()
for (i in 1:7){
  ICS_AUC_JP_lda[i]<-simple_auc(ROC_ICS_justpass_TPR_lda[,i],ROC_ICS_justpass_FPR_lda[,i])
}
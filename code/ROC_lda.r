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
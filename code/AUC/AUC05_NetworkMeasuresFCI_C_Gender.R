##THIS FILE WILL BE SOURCED IN successPlots.r


#######################################################
###LOAD CALCULATIONS FROM log reg, lda, qdua and KNN###
#######################################################
load("data/ROC05_NetworkMeasuresFCI_C_gender_logreg.Rdata")
load("data/ROC05_NetworkMeasuresFCI_C_gender_lda.Rdata")
load("data/ROC05_NetworkMeasuresFCI_C_gender_qda.Rdata")
load("data/ROC05_NetworkMeasuresFCI_C_gender_knn.Rdata")

#OUTCOME = PASSED
#Logistic Regression

ROC_PS_log_pfm<-performanceMeasures(ROC_PS_log)
PS_AUC_NetworkMeasuresFCI_C_gender_log_all<-AUC(ROC_PS_log_pfm[[1]],ROC_PS_log_pfm[[2]])

ROC_CD_log_pfm<-performanceMeasures(ROC_CD_log)
CD_AUC_NetworkMeasuresFCI_C_gender_log_all<-AUC(ROC_CD_log_pfm[[1]],ROC_CD_log_pfm[[2]])

ROC_ICS_log_pfm<-performanceMeasures(ROC_ICS_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_log_all<-AUC(ROC_ICS_log_pfm[[1]],ROC_ICS_log_pfm[[2]])

ROC_PS_log_pfm_PRTE<-performanceMeasures(ROC_PS_log_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_log_PRTE<-AUC(ROC_PS_log_pfm_PRTE[[1]],ROC_PS_log_pfm_PRTE[[2]])

ROC_CD_log_pfm_PRTE<-performanceMeasures(ROC_CD_log_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_log_PRTE<-AUC(ROC_CD_log_pfm_PRTE[[1]],ROC_CD_log_pfm_PRTE[[2]])

ROC_ICS_log_pfm_PRTE<-performanceMeasures(ROC_ICS_log_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_log_PRTE<-AUC(ROC_ICS_log_pfm_PRTE[[1]],ROC_ICS_log_pfm_PRTE[[2]])


ROC_PS_log_pfm_PRH<-performanceMeasures(ROC_PS_log_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_log_PRH<-AUC(ROC_PS_log_pfm_PRH[[1]],ROC_PS_log_pfm_PRH[[2]])

ROC_CD_log_pfm_PRH<-performanceMeasures(ROC_CD_log_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_log_PRH<-AUC(ROC_CD_log_pfm_PRH[[1]],ROC_CD_log_pfm_PRH[[2]])

ROC_ICS_log_pfm_PRH<-performanceMeasures(ROC_ICS_log_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_log_PRH<-AUC(ROC_ICS_log_pfm_PRH[[1]],ROC_ICS_log_pfm_PRH[[2]])


ROC_PS_log_pfm_TEH<-performanceMeasures(ROC_PS_log_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_log_TEH<-AUC(ROC_PS_log_pfm_TEH[[1]],ROC_PS_log_pfm_TEH[[2]])

ROC_CD_log_pfm_TEH<-performanceMeasures(ROC_CD_log_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_log_TEH<-AUC(ROC_CD_log_pfm_TEH[[1]],ROC_CD_log_pfm_TEH[[2]])

ROC_ICS_log_pfm_TEH<-performanceMeasures(ROC_ICS_log_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_log_TEH<-AUC(ROC_ICS_log_pfm_TEH[[1]],ROC_ICS_log_pfm_TEH[[2]])

ROC_PS_log_pfm_PR<-performanceMeasures(ROC_PS_log_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_log_PR<-AUC(ROC_PS_log_pfm_PR[[1]],ROC_PS_log_pfm_PR[[2]])

ROC_CD_log_pfm_PR<-performanceMeasures(ROC_CD_log_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_log_PR<-AUC(ROC_CD_log_pfm_PR[[1]],ROC_CD_log_pfm_PR[[2]])

ROC_ICS_log_pfm_PR<-performanceMeasures(ROC_ICS_log_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_log_PR<-AUC(ROC_ICS_log_pfm_PR[[1]],ROC_ICS_log_pfm_PR[[2]])

ROC_PS_log_pfm_TE<-performanceMeasures(ROC_PS_log_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_log_TE<-AUC(ROC_PS_log_pfm_TE[[1]],ROC_PS_log_pfm_TE[[2]])

ROC_CD_log_pfm_TE<-performanceMeasures(ROC_CD_log_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_log_TE<-AUC(ROC_CD_log_pfm_TE[[1]],ROC_CD_log_pfm_TE[[2]])

ROC_ICS_log_pfm_TE<-performanceMeasures(ROC_ICS_log_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_log_TE<-AUC(ROC_ICS_log_pfm_TE[[1]],ROC_ICS_log_pfm_TE[[2]])

ROC_PS_log_pfm_H<-performanceMeasures(ROC_PS_log_H)
PS_AUC_NetworkMeasuresFCI_C_gender_log_H<-AUC(ROC_PS_log_pfm_H[[1]],ROC_PS_log_pfm_H[[2]])

ROC_CD_log_pfm_H<-performanceMeasures(ROC_CD_log_H)
CD_AUC_NetworkMeasuresFCI_C_gender_log_H<-AUC(ROC_CD_log_pfm_H[[1]],ROC_CD_log_pfm_H[[2]])

ROC_ICS_log_pfm_H<-performanceMeasures(ROC_ICS_log_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_log_H<-AUC(ROC_ICS_log_pfm_H[[1]],ROC_ICS_log_pfm_H[[2]])


#########LDA

ROC_PS_lda_pfm<-performanceMeasures(ROC_PS_log)
PS_AUC_NetworkMeasuresFCI_C_gender_lda_all<-AUC(ROC_PS_lda_pfm[[1]],ROC_PS_lda_pfm[[2]])

ROC_CD_lda_pfm<-performanceMeasures(ROC_CD_log)
CD_AUC_NetworkMeasuresFCI_C_gender_lda_all<-AUC(ROC_CD_lda_pfm[[1]],ROC_CD_lda_pfm[[2]])

ROC_ICS_lda_pfm<-performanceMeasures(ROC_ICS_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_lda_all<-AUC(ROC_ICS_lda_pfm[[1]],ROC_ICS_lda_pfm[[2]])

ROC_PS_lda_pfm_PRTE<-performanceMeasures(ROC_PS_lda_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_lda_PRTE<-AUC(ROC_PS_lda_pfm_PRTE[[1]],ROC_PS_lda_pfm_PRTE[[2]])

ROC_CD_lda_pfm_PRTE<-performanceMeasures(ROC_CD_lda_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_lda_PRTE<-AUC(ROC_CD_lda_pfm_PRTE[[1]],ROC_CD_lda_pfm_PRTE[[2]])

ROC_ICS_lda_pfm_PRTE<-performanceMeasures(ROC_ICS_lda_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_lda_PRTE<-AUC(ROC_ICS_lda_pfm_PRTE[[1]],ROC_ICS_lda_pfm_PRTE[[2]])


ROC_PS_lda_pfm_PRH<-performanceMeasures(ROC_PS_lda_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_lda_PRH<-AUC(ROC_PS_lda_pfm_PRH[[1]],ROC_PS_lda_pfm_PRH[[2]])

ROC_CD_lda_pfm_PRH<-performanceMeasures(ROC_CD_lda_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_lda_PRH<-AUC(ROC_CD_lda_pfm_PRH[[1]],ROC_CD_lda_pfm_PRH[[2]])

ROC_ICS_lda_pfm_PRH<-performanceMeasures(ROC_ICS_lda_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_lda_PRH<-AUC(ROC_ICS_lda_pfm_PRH[[1]],ROC_ICS_lda_pfm_PRH[[2]])


ROC_PS_lda_pfm_TEH<-performanceMeasures(ROC_PS_lda_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_lda_TEH<-AUC(ROC_PS_lda_pfm_TEH[[1]],ROC_PS_lda_pfm_TEH[[2]])

ROC_CD_lda_pfm_TEH<-performanceMeasures(ROC_CD_lda_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_lda_TEH<-AUC(ROC_CD_lda_pfm_TEH[[1]],ROC_CD_lda_pfm_TEH[[2]])

ROC_ICS_lda_pfm_TEH<-performanceMeasures(ROC_ICS_lda_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_lda_TEH<-AUC(ROC_ICS_lda_pfm_TEH[[1]],ROC_ICS_lda_pfm_TEH[[2]])

ROC_PS_lda_pfm_PR<-performanceMeasures(ROC_PS_lda_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_lda_PR<-AUC(ROC_PS_lda_pfm_PR[[1]],ROC_PS_lda_pfm_PR[[2]])

ROC_CD_lda_pfm_PR<-performanceMeasures(ROC_CD_lda_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_lda_PR<-AUC(ROC_CD_lda_pfm_PR[[1]],ROC_CD_lda_pfm_PR[[2]])

ROC_ICS_lda_pfm_PR<-performanceMeasures(ROC_ICS_lda_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_lda_PR<-AUC(ROC_ICS_lda_pfm_PR[[1]],ROC_ICS_lda_pfm_PR[[2]])

ROC_PS_lda_pfm_TE<-performanceMeasures(ROC_PS_lda_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_lda_TE<-AUC(ROC_PS_lda_pfm_TE[[1]],ROC_PS_lda_pfm_TE[[2]])

ROC_CD_lda_pfm_TE<-performanceMeasures(ROC_CD_lda_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_lda_TE<-AUC(ROC_CD_lda_pfm_TE[[1]],ROC_CD_lda_pfm_TE[[2]])

ROC_ICS_lda_pfm_TE<-performanceMeasures(ROC_ICS_lda_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_lda_TE<-AUC(ROC_ICS_lda_pfm_TE[[1]],ROC_ICS_lda_pfm_TE[[2]])

ROC_PS_lda_pfm_H<-performanceMeasures(ROC_PS_lda_H)
PS_AUC_NetworkMeasuresFCI_C_gender_lda_H<-AUC(ROC_PS_lda_pfm_H[[1]],ROC_PS_lda_pfm_H[[2]])

ROC_CD_lda_pfm_H<-performanceMeasures(ROC_CD_lda_H)
CD_AUC_NetworkMeasuresFCI_C_gender_lda_H<-AUC(ROC_CD_lda_pfm_H[[1]],ROC_CD_lda_pfm_H[[2]])

ROC_ICS_lda_pfm_H<-performanceMeasures(ROC_ICS_lda_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_lda_H<-AUC(ROC_ICS_lda_pfm_H[[1]],ROC_ICS_lda_pfm_H[[2]])

#QDA

ROC_PS_qda_pfm<-performanceMeasures(ROC_PS_log)
PS_AUC_NetworkMeasuresFCI_C_gender_qda_all<-AUC(ROC_PS_qda_pfm[[1]],ROC_PS_qda_pfm[[2]])

ROC_CD_qda_pfm<-performanceMeasures(ROC_CD_log)
CD_AUC_NetworkMeasuresFCI_C_gender_qda_all<-AUC(ROC_CD_qda_pfm[[1]],ROC_CD_qda_pfm[[2]])

ROC_ICS_qda_pfm<-performanceMeasures(ROC_ICS_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_qda_all<-AUC(ROC_ICS_qda_pfm[[1]],ROC_ICS_qda_pfm[[2]])

ROC_PS_qda_pfm_PRTE<-performanceMeasures(ROC_PS_qda_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_qda_PRTE<-AUC(ROC_PS_qda_pfm_PRTE[[1]],ROC_PS_qda_pfm_PRTE[[2]])

ROC_CD_qda_pfm_PRTE<-performanceMeasures(ROC_CD_qda_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_qda_PRTE<-AUC(ROC_CD_qda_pfm_PRTE[[1]],ROC_CD_qda_pfm_PRTE[[2]])

ROC_ICS_qda_pfm_PRTE<-performanceMeasures(ROC_ICS_qda_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_qda_PRTE<-AUC(ROC_ICS_qda_pfm_PRTE[[1]],ROC_ICS_qda_pfm_PRTE[[2]])


ROC_PS_qda_pfm_PRH<-performanceMeasures(ROC_PS_qda_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_qda_PRH<-AUC(ROC_PS_qda_pfm_PRH[[1]],ROC_PS_qda_pfm_PRH[[2]])

ROC_CD_qda_pfm_PRH<-performanceMeasures(ROC_CD_qda_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_qda_PRH<-AUC(ROC_CD_qda_pfm_PRH[[1]],ROC_CD_qda_pfm_PRH[[2]])

ROC_ICS_qda_pfm_PRH<-performanceMeasures(ROC_ICS_qda_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_qda_PRH<-AUC(ROC_ICS_qda_pfm_PRH[[1]],ROC_ICS_qda_pfm_PRH[[2]])


ROC_PS_qda_pfm_TEH<-performanceMeasures(ROC_PS_qda_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_qda_TEH<-AUC(ROC_PS_qda_pfm_TEH[[1]],ROC_PS_qda_pfm_TEH[[2]])

ROC_CD_qda_pfm_TEH<-performanceMeasures(ROC_CD_qda_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_qda_TEH<-AUC(ROC_CD_qda_pfm_TEH[[1]],ROC_CD_qda_pfm_TEH[[2]])

ROC_ICS_qda_pfm_TEH<-performanceMeasures(ROC_ICS_qda_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_qda_TEH<-AUC(ROC_ICS_qda_pfm_TEH[[1]],ROC_ICS_qda_pfm_TEH[[2]])

ROC_PS_qda_pfm_PR<-performanceMeasures(ROC_PS_qda_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_qda_PR<-AUC(ROC_PS_qda_pfm_PR[[1]],ROC_PS_qda_pfm_PR[[2]])

ROC_CD_qda_pfm_PR<-performanceMeasures(ROC_CD_qda_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_qda_PR<-AUC(ROC_CD_qda_pfm_PR[[1]],ROC_CD_qda_pfm_PR[[2]])

ROC_ICS_qda_pfm_PR<-performanceMeasures(ROC_ICS_qda_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_qda_PR<-AUC(ROC_ICS_qda_pfm_PR[[1]],ROC_ICS_qda_pfm_PR[[2]])

ROC_PS_qda_pfm_TE<-performanceMeasures(ROC_PS_qda_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_qda_TE<-AUC(ROC_PS_qda_pfm_TE[[1]],ROC_PS_qda_pfm_TE[[2]])

ROC_CD_qda_pfm_TE<-performanceMeasures(ROC_CD_qda_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_qda_TE<-AUC(ROC_CD_qda_pfm_TE[[1]],ROC_CD_qda_pfm_TE[[2]])

ROC_ICS_qda_pfm_TE<-performanceMeasures(ROC_ICS_qda_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_qda_TE<-AUC(ROC_ICS_qda_pfm_TE[[1]],ROC_ICS_qda_pfm_TE[[2]])

ROC_PS_qda_pfm_H<-performanceMeasures(ROC_PS_qda_H)
PS_AUC_NetworkMeasuresFCI_C_gender_qda_H<-AUC(ROC_PS_qda_pfm_H[[1]],ROC_PS_qda_pfm_H[[2]])

ROC_CD_qda_pfm_H<-performanceMeasures(ROC_CD_qda_H)
CD_AUC_NetworkMeasuresFCI_C_gender_qda_H<-AUC(ROC_CD_qda_pfm_H[[1]],ROC_CD_qda_pfm_H[[2]])

ROC_ICS_qda_pfm_H<-performanceMeasures(ROC_ICS_qda_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_qda_H<-AUC(ROC_ICS_qda_pfm_H[[1]],ROC_ICS_qda_pfm_H[[2]])

#KNN
ROC_PS_knn_pfm<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_knn_all<-colMeans(ROC_PS_knn_pfm[[4]])

ROC_CD_knn_pfm<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_knn_all<-colMeans(ROC_CD_knn_pfm[[4]])

ROC_ICS_knn_pfm<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_knn_all<-colMeans(ROC_ICS_knn_pfm[[4]])

ROC_PS_knn_pfm_PRTE<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_knn_PRTE<-colMeans(ROC_PS_knn_pfm_PRTE[[4]])

ROC_CD_knn_pfm_PRTE<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_knn_PRTE<-colMeans(ROC_CD_knn_pfm_PRTE[[4]])

ROC_ICS_knn_pfm_PRTE<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_knn_PRTE<-colMeans(ROC_ICS_knn_pfm_PRTE[[4]])

ROC_PS_knn_pfm_PRH<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_knn_PRH<-colMeans(ROC_PS_knn_pfm_PRH[[4]])

ROC_CD_knn_pfm_PRH<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_knn_PRH<-colMeans(ROC_CD_knn_pfm_PRH[[4]])

ROC_ICS_knn_pfm_PRH<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_knn_PRH<-colMeans(ROC_ICS_knn_pfm_PRH[[4]])

ROC_PS_knn_pfm_TEH<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_knn_TEH<-colMeans(ROC_PS_knn_pfm_TEH[[4]])

ROC_CD_knn_pfm_TEH<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_knn_TEH<-colMeans(ROC_CD_knn_pfm_TEH[[4]])

ROC_ICS_knn_pfm_TEH<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_knn_TEH<-colMeans(ROC_ICS_knn_pfm_TEH[[4]])

ROC_PS_knn_pfm_PR<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_knn_PR<-colMeans(ROC_PS_knn_pfm_PR[[4]])

ROC_CD_knn_pfm_PR<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_knn_PR<-colMeans(ROC_CD_knn_pfm_PR[[4]])

ROC_ICS_knn_pfm_PR<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_knn_PR<-colMeans(ROC_ICS_knn_pfm_PR[[4]])

ROC_PS_knn_pfm_TE<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_knn_TE<-colMeans(ROC_PS_knn_pfm_TE[[4]])

ROC_CD_knn_pfm_TE<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_knn_TE<-colMeans(ROC_CD_knn_pfm_TE[[4]])

ROC_ICS_knn_pfm_TE<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_knn_TE<-colMeans(ROC_ICS_knn_pfm_TE[[4]])

ROC_PS_knn_pfm_H<-performanceMeasures(ROC_PS_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_knn_all<-colMeans(ROC_PS_knn_pfm_H[[4]])

ROC_CD_knn_pfm_H<-performanceMeasures(ROC_CD_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_knn_H<-colMeans(ROC_CD_knn_pfm_H[[4]])

ROC_ICS_knn_pfm_H<-performanceMeasures(ROC_ICS_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_knn_H<-colMeans(ROC_ICS_knn_pfm_H[[4]])

#OUTCOME = JUSTPASSED
#Logistic Regression

ROC_PS_justpass_log_pfm<-performanceMeasures(ROC_PS_justpass_log)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_all<-AUC(ROC_PS_justpass_log_pfm[[1]],ROC_PS_justpass_log_pfm[[2]])

ROC_CD_justpass_log_pfm<-performanceMeasures(ROC_CD_justpass_log)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_log_all<-AUC(ROC_CD_justpass_log_pfm[[1]],ROC_CD_justpass_log_pfm[[2]])

ROC_ICS_justpass_log_pfm<-performanceMeasures(ROC_ICS_justpass_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_all<-AUC(ROC_ICS_justpass_log_pfm[[1]],ROC_ICS_justpass_log_pfm[[2]])



ROC_PS_justpass_log_pfm_PRTE<-performanceMeasures(ROC_PS_justpass_log_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_PRTE<-AUC(ROC_PS_justpass_log_pfm_PRTE[[1]],ROC_PS_justpass_log_pfm_PRTE[[2]])

ROC_CD_justpass_log_pfm_PRTE<-performanceMeasures(ROC_CD_justpass_log_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_log_PRTE<-AUC(ROC_CD_justpass_log_pfm_PRTE[[1]],ROC_CD_justpass_log_pfm_PRTE[[2]])

ROC_ICS_justpass_log_pfm_PRTE<-performanceMeasures(ROC_ICS_justpass_log_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_PRTE<-AUC(ROC_ICS_justpass_log_pfm_PRTE[[1]],ROC_ICS_justpass_log_pfm_PRTE[[2]])


ROC_PS_justpass_log_pfm_PRH<-performanceMeasures(ROC_PS_justpass_log_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_PRH<-AUC(ROC_PS_justpass_log_pfm_PRH[[1]],ROC_PS_justpass_log_pfm_PRH[[2]])

ROC_CD_justpass_log_pfm_PRH<-performanceMeasures(ROC_CD_justpass_log_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_log_PRH<-AUC(ROC_CD_justpass_log_pfm_PRH[[1]],ROC_CD_justpass_log_pfm_PRH[[2]])

ROC_ICS_justpass_log_pfm_PRH<-performanceMeasures(ROC_ICS_justpass_log_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_PRH<-AUC(ROC_ICS_justpass_log_pfm_PRH[[1]],ROC_ICS_justpass_log_pfm_PRH[[2]])


ROC_PS_justpass_log_pfm_TEH<-performanceMeasures(ROC_PS_justpass_log_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_TEH<-AUC(ROC_PS_justpass_log_pfm_TEH[[1]],ROC_PS_justpass_log_pfm_TEH[[2]])

ROC_CD_justpass_log_pfm_TEH<-performanceMeasures(ROC_CD_justpass_log_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_log_TEH<-AUC(ROC_CD_justpass_log_pfm_TEH[[1]],ROC_CD_justpass_log_pfm_TEH[[2]])

ROC_ICS_justpass_log_pfm_TEH<-performanceMeasures(ROC_ICS_justpass_log_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_TEH<-AUC(ROC_ICS_justpass_log_pfm_TEH[[1]],ROC_ICS_justpass_log_pfm_TEH[[2]])

ROC_PS_justpass_log_pfm_PR<-performanceMeasures(ROC_PS_justpass_log_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_PR<-AUC(ROC_PS_justpass_log_pfm_PR[[1]],ROC_PS_justpass_log_pfm_PR[[2]])

ROC_CD_justpass_log_pfm_PR<-performanceMeasures(ROC_CD_justpass_log_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_log_PR<-AUC(ROC_CD_justpass_log_pfm_PR[[1]],ROC_CD_justpass_log_pfm_PR[[2]])

ROC_ICS_justpass_log_pfm_PR<-performanceMeasures(ROC_ICS_justpass_log_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_PR<-AUC(ROC_ICS_justpass_log_pfm_PR[[1]],ROC_ICS_justpass_log_pfm_PR[[2]])

ROC_PS_justpass_log_pfm_TE<-performanceMeasures(ROC_PS_justpass_log_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_TE<-AUC(ROC_PS_justpass_log_pfm_TE[[1]],ROC_PS_justpass_log_pfm_TE[[2]])

ROC_CD_justpass_log_pfm_TE<-performanceMeasures(ROC_CD_justpass_log_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_log_TE<-AUC(ROC_CD_justpass_log_pfm_TE[[1]],ROC_CD_justpass_log_pfm_TE[[2]])

ROC_ICS_justpass_log_pfm_TE<-performanceMeasures(ROC_ICS_justpass_log_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_TE<-AUC(ROC_ICS_justpass_log_pfm_TE[[1]],ROC_ICS_justpass_log_pfm_TE[[2]])

ROC_PS_justpass_log_pfm_H<-performanceMeasures(ROC_PS_justpass_log_H)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_H<-AUC(ROC_PS_justpass_log_pfm_H[[1]],ROC_PS_justpass_log_pfm_H[[2]])

ROC_CD_justpass_log_pfm_H<-performanceMeasures(ROC_CD_justpass_log_H)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_log_H<-AUC(ROC_CD_justpass_log_pfm_H[[1]],ROC_CD_justpass_log_pfm_H[[2]])

ROC_ICS_justpass_log_pfm_H<-performanceMeasures(ROC_ICS_justpass_log_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_log_H<-AUC(ROC_ICS_justpass_log_pfm_H[[1]],ROC_ICS_justpass_log_pfm_H[[2]])


#########LDA

ROC_PS_justpass_lda_pfm<-performanceMeasures(ROC_PS_justpass_log)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_all<-AUC(ROC_PS_justpass_lda_pfm[[1]],ROC_PS_justpass_lda_pfm[[2]])

ROC_CD_justpass_lda_pfm<-performanceMeasures(ROC_CD_justpass_log)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_all<-AUC(ROC_CD_justpass_lda_pfm[[1]],ROC_CD_justpass_lda_pfm[[2]])

ROC_ICS_justpass_lda_pfm<-performanceMeasures(ROC_ICS_justpass_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_all<-AUC(ROC_ICS_justpass_lda_pfm[[1]],ROC_ICS_justpass_lda_pfm[[2]])

ROC_PS_justpass_lda_pfm_PRTE<-performanceMeasures(ROC_PS_justpass_lda_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_PRTE<-AUC(ROC_PS_justpass_lda_pfm_PRTE[[1]],ROC_PS_justpass_lda_pfm_PRTE[[2]])

ROC_CD_justpass_lda_pfm_PRTE<-performanceMeasures(ROC_CD_justpass_lda_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_PRTE<-AUC(ROC_CD_justpass_lda_pfm_PRTE[[1]],ROC_CD_justpass_lda_pfm_PRTE[[2]])

ROC_ICS_justpass_lda_pfm_PRTE<-performanceMeasures(ROC_ICS_justpass_lda_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_PRTE<-AUC(ROC_ICS_justpass_lda_pfm_PRTE[[1]],ROC_ICS_justpass_lda_pfm_PRTE[[2]])


ROC_PS_justpass_lda_pfm_PRH<-performanceMeasures(ROC_PS_justpass_lda_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_PRH<-AUC(ROC_PS_justpass_lda_pfm_PRH[[1]],ROC_PS_justpass_lda_pfm_PRH[[2]])

ROC_CD_justpass_lda_pfm_PRH<-performanceMeasures(ROC_CD_justpass_lda_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_PRH<-AUC(ROC_CD_justpass_lda_pfm_PRH[[1]],ROC_CD_justpass_lda_pfm_PRH[[2]])

ROC_ICS_justpass_lda_pfm_PRH<-performanceMeasures(ROC_ICS_justpass_lda_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_PRH<-AUC(ROC_ICS_justpass_lda_pfm_PRH[[1]],ROC_ICS_justpass_lda_pfm_PRH[[2]])


ROC_PS_justpass_lda_pfm_TEH<-performanceMeasures(ROC_PS_justpass_lda_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_TEH<-AUC(ROC_PS_justpass_lda_pfm_TEH[[1]],ROC_PS_justpass_lda_pfm_TEH[[2]])

ROC_CD_justpass_lda_pfm_TEH<-performanceMeasures(ROC_CD_justpass_lda_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_TEH<-AUC(ROC_CD_justpass_lda_pfm_TEH[[1]],ROC_CD_justpass_lda_pfm_TEH[[2]])

ROC_ICS_justpass_lda_pfm_TEH<-performanceMeasures(ROC_ICS_justpass_lda_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_TEH<-AUC(ROC_ICS_justpass_lda_pfm_TEH[[1]],ROC_ICS_justpass_lda_pfm_TEH[[2]])

ROC_PS_justpass_lda_pfm_PR<-performanceMeasures(ROC_PS_justpass_lda_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_PR<-AUC(ROC_PS_justpass_lda_pfm_PR[[1]],ROC_PS_justpass_lda_pfm_PR[[2]])

ROC_CD_justpass_lda_pfm_PR<-performanceMeasures(ROC_CD_justpass_lda_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_PR<-AUC(ROC_CD_justpass_lda_pfm_PR[[1]],ROC_CD_justpass_lda_pfm_PR[[2]])

ROC_ICS_justpass_lda_pfm_PR<-performanceMeasures(ROC_ICS_justpass_lda_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_PR<-AUC(ROC_ICS_justpass_lda_pfm_PR[[1]],ROC_ICS_justpass_lda_pfm_PR[[2]])

ROC_PS_justpass_lda_pfm_TE<-performanceMeasures(ROC_PS_justpass_lda_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_TE<-AUC(ROC_PS_justpass_lda_pfm_TE[[1]],ROC_PS_justpass_lda_pfm_TE[[2]])

ROC_CD_justpass_lda_pfm_TE<-performanceMeasures(ROC_CD_justpass_lda_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_TE<-AUC(ROC_CD_justpass_lda_pfm_TE[[1]],ROC_CD_justpass_lda_pfm_TE[[2]])

ROC_ICS_justpass_lda_pfm_TE<-performanceMeasures(ROC_ICS_justpass_lda_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_TE<-AUC(ROC_ICS_justpass_lda_pfm_TE[[1]],ROC_ICS_justpass_lda_pfm_TE[[2]])

ROC_PS_justpass_lda_pfm_H<-performanceMeasures(ROC_PS_justpass_lda_H)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_H<-AUC(ROC_PS_justpass_lda_pfm_H[[1]],ROC_PS_justpass_lda_pfm_H[[2]])

ROC_CD_justpass_lda_pfm_H<-performanceMeasures(ROC_CD_justpass_lda_H)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_H<-AUC(ROC_CD_justpass_lda_pfm_H[[1]],ROC_CD_justpass_lda_pfm_H[[2]])

ROC_ICS_justpass_lda_pfm_H<-performanceMeasures(ROC_ICS_justpass_lda_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_lda_H<-AUC(ROC_ICS_justpass_lda_pfm_H[[1]],ROC_ICS_justpass_lda_pfm_H[[2]])

#QDA

ROC_PS_justpass_qda_pfm<-performanceMeasures(ROC_PS_justpass_log)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_all<-AUC(ROC_PS_justpass_qda_pfm[[1]],ROC_PS_justpass_qda_pfm[[2]])

ROC_CD_justpass_qda_pfm<-performanceMeasures(ROC_CD_justpass_log)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_all<-AUC(ROC_CD_justpass_qda_pfm[[1]],ROC_CD_justpass_qda_pfm[[2]])

ROC_ICS_justpass_qda_pfm<-performanceMeasures(ROC_ICS_justpass_log)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_all<-AUC(ROC_ICS_justpass_qda_pfm[[1]],ROC_ICS_justpass_qda_pfm[[2]])

ROC_PS_justpass_qda_pfm_PRTE<-performanceMeasures(ROC_PS_justpass_qda_PRTE)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_PRTE<-AUC(ROC_PS_justpass_qda_pfm_PRTE[[1]],ROC_PS_justpass_qda_pfm_PRTE[[2]])

ROC_CD_justpass_qda_pfm_PRTE<-performanceMeasures(ROC_CD_justpass_qda_PRTE)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_PRTE<-AUC(ROC_CD_justpass_qda_pfm_PRTE[[1]],ROC_CD_justpass_qda_pfm_PRTE[[2]])

ROC_ICS_justpass_qda_pfm_PRTE<-performanceMeasures(ROC_ICS_justpass_qda_PRTE)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_PRTE<-AUC(ROC_ICS_justpass_qda_pfm_PRTE[[1]],ROC_ICS_justpass_qda_pfm_PRTE[[2]])


ROC_PS_justpass_qda_pfm_PRH<-performanceMeasures(ROC_PS_justpass_qda_PRH)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_PRH<-AUC(ROC_PS_justpass_qda_pfm_PRH[[1]],ROC_PS_justpass_qda_pfm_PRH[[2]])

ROC_CD_justpass_qda_pfm_PRH<-performanceMeasures(ROC_CD_justpass_qda_PRH)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_PRH<-AUC(ROC_CD_justpass_qda_pfm_PRH[[1]],ROC_CD_justpass_qda_pfm_PRH[[2]])

ROC_ICS_justpass_qda_pfm_PRH<-performanceMeasures(ROC_ICS_justpass_qda_PRH)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_PRH<-AUC(ROC_ICS_justpass_qda_pfm_PRH[[1]],ROC_ICS_justpass_qda_pfm_PRH[[2]])


ROC_PS_justpass_qda_pfm_TEH<-performanceMeasures(ROC_PS_justpass_qda_TEH)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_TEH<-AUC(ROC_PS_justpass_qda_pfm_TEH[[1]],ROC_PS_justpass_qda_pfm_TEH[[2]])

ROC_CD_justpass_qda_pfm_TEH<-performanceMeasures(ROC_CD_justpass_qda_TEH)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_TEH<-AUC(ROC_CD_justpass_qda_pfm_TEH[[1]],ROC_CD_justpass_qda_pfm_TEH[[2]])

ROC_ICS_justpass_qda_pfm_TEH<-performanceMeasures(ROC_ICS_justpass_qda_TEH)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_TEH<-AUC(ROC_ICS_justpass_qda_pfm_TEH[[1]],ROC_ICS_justpass_qda_pfm_TEH[[2]])

ROC_PS_justpass_qda_pfm_PR<-performanceMeasures(ROC_PS_justpass_qda_PR)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_PR<-AUC(ROC_PS_justpass_qda_pfm_PR[[1]],ROC_PS_justpass_qda_pfm_PR[[2]])

ROC_CD_justpass_qda_pfm_PR<-performanceMeasures(ROC_CD_justpass_qda_PR)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_PR<-AUC(ROC_CD_justpass_qda_pfm_PR[[1]],ROC_CD_justpass_qda_pfm_PR[[2]])

ROC_ICS_justpass_qda_pfm_PR<-performanceMeasures(ROC_ICS_justpass_qda_PR)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_PR<-AUC(ROC_ICS_justpass_qda_pfm_PR[[1]],ROC_ICS_justpass_qda_pfm_PR[[2]])

ROC_PS_justpass_qda_pfm_TE<-performanceMeasures(ROC_PS_justpass_qda_TE)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_TE<-AUC(ROC_PS_justpass_qda_pfm_TE[[1]],ROC_PS_justpass_qda_pfm_TE[[2]])

ROC_CD_justpass_qda_pfm_TE<-performanceMeasures(ROC_CD_justpass_qda_TE)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_TE<-AUC(ROC_CD_justpass_qda_pfm_TE[[1]],ROC_CD_justpass_qda_pfm_TE[[2]])

ROC_ICS_justpass_qda_pfm_TE<-performanceMeasures(ROC_ICS_justpass_qda_TE)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_TE<-AUC(ROC_ICS_justpass_qda_pfm_TE[[1]],ROC_ICS_justpass_qda_pfm_TE[[2]])

ROC_PS_justpass_qda_pfm_H<-performanceMeasures(ROC_PS_justpass_qda_H)
PS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_H<-AUC(ROC_PS_justpass_qda_pfm_H[[1]],ROC_PS_justpass_qda_pfm_H[[2]])

ROC_CD_justpass_qda_pfm_H<-performanceMeasures(ROC_CD_justpass_qda_H)
CD_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_H<-AUC(ROC_CD_justpass_qda_pfm_H[[1]],ROC_CD_justpass_qda_pfm_H[[2]])

ROC_ICS_justpass_qda_pfm_H<-performanceMeasures(ROC_ICS_justpass_qda_H)
ICS_AUC_NetworkMeasuresFCI_C_gender_justpass_qda_H<-AUC(ROC_ICS_justpass_qda_pfm_H[[1]],ROC_ICS_justpass_qda_pfm_H[[2]])

#KNN
ROC_PS_justpass_knn_pfm<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_all<-colMeans(ROC_PS_justpass_knn_pfm[[4]])

ROC_CD_justpass_knn_pfm<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_all<-colMeans(ROC_CD_justpass_knn_pfm[[4]])

ROC_ICS_justpass_knn_pfm<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_all<-colMeans(ROC_ICS_justpass_knn_pfm[[4]])

ROC_PS_justpass_knn_pfm_PRTE<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_PRTE<-colMeans(ROC_PS_justpass_knn_pfm_PRTE[[4]])

ROC_CD_justpass_knn_pfm_PRTE<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_PRTE<-colMeans(ROC_CD_justpass_knn_pfm_PRTE[[4]])

ROC_ICS_justpass_knn_pfm_PRTE<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_PRTE<-colMeans(ROC_ICS_justpass_knn_pfm_PRTE[[4]])

ROC_PS_justpass_knn_pfm_PRH<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_PRH<-colMeans(ROC_PS_justpass_knn_pfm_PRH[[4]])

ROC_CD_justpass_knn_pfm_PRH<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_PRH<-colMeans(ROC_CD_justpass_knn_pfm_PRH[[4]])

ROC_ICS_justpass_knn_pfm_PRH<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_PRH<-colMeans(ROC_ICS_justpass_knn_pfm_PRH[[4]])

ROC_PS_justpass_knn_pfm_TEH<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_TEH<-colMeans(ROC_PS_justpass_knn_pfm_TEH[[4]])

ROC_CD_justpass_knn_pfm_TEH<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_TEH<-colMeans(ROC_CD_justpass_knn_pfm_TEH[[4]])

ROC_ICS_justpass_knn_pfm_TEH<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_TEH<-colMeans(ROC_ICS_justpass_knn_pfm_TEH[[4]])

ROC_PS_justpass_knn_pfm_PR<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_PR<-colMeans(ROC_PS_justpass_knn_pfm_PR[[4]])

ROC_CD_justpass_knn_pfm_PR<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_PR<-colMeans(ROC_CD_justpass_knn_pfm_PR[[4]])

ROC_ICS_justpass_knn_pfm_PR<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_PR<-colMeans(ROC_ICS_justpass_knn_pfm_PR[[4]])

ROC_PS_justpass_knn_pfm_TE<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_TE<-colMeans(ROC_PS_justpass_knn_pfm_TE[[4]])

ROC_CD_justpass_knn_pfm_TE<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_TE<-colMeans(ROC_CD_justpass_knn_pfm_TE[[4]])

ROC_ICS_justpass_knn_pfm_TE<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_TE<-colMeans(ROC_ICS_justpass_knn_pfm_TE[[4]])

ROC_PS_justpass_knn_pfm_H<-performanceMeasures(ROC_PS_justpass_knn,M=20)
PS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_all<-colMeans(ROC_PS_justpass_knn_pfm_H[[4]])

ROC_CD_justpass_knn_pfm_H<-performanceMeasures(ROC_CD_justpass_knn,M=20)
CD_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_H<-colMeans(ROC_CD_justpass_knn_pfm_H[[4]])

ROC_ICS_justpass_knn_pfm_H<-performanceMeasures(ROC_ICS_justpass_knn,M=20)
ICS_mSR_NetworkMeasuresFCI_C_gender_justpass_knn_H<-colMeans(ROC_ICS_justpass_knn_pfm_H[[4]])

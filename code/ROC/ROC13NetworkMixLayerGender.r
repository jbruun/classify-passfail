## Models without background measures

rm(list = ls())
tstart<-Sys.time()

library(igraph)
library(dplyr)
library(class)   # for knn
library(tidyr)
library(ggplot2)  # for plotting success rates
library(MASS)

load("data/centrality_data_frames.Rdata")
# Turn long data frame into list of weekly frames
dfBG<-dfPS[,1:13] #dataframe with background variables
dfPSC<-data.frame(PS_PageRank=dfPS$PageRank,PS_tarEnt=dfPS$tarEnt,PS_Hide=dfPS$Hide) #dataframe with network centralities for PS layer
dfCDC<-data.frame(CD_PageRank=dfCD$PageRank,CD_tarEnt=dfCD$tarEnt,CD_Hide=dfCD$Hide) #dataframe with network centralities for CD layer
dfICSC<-data.frame(ICS_PageRank=dfICS$PageRank,ICS_tarEnt=dfICS$tarEnt,ICS_Hide=dfICS$Hide) #dataframe with network centralities for ICS layer
dfAll<-data.frame(dfBG,dfPSC,dfCDC,dfICSC) #All combined
centAll <- dfAll %>% group_split(Week)


source("code/jackknife_functions.R")
source("code/ROC_functions.R")

#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########
#########LOGISTIC REGRESSION###########

######BEST NETWORK PREDICTORS PASSED######

ROC_A_log<-list() #Model A: PS_PR+H + ICS_PR + CD_PR
for(i in 1:100){
  predA_x<-jackPredLog(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predA_x)
  ROC_A_log[[i]]<-ROC
}

ROC_B_log<-list() #Model B: PS_PR+H + ICS_PR + CD_PR+TE
for(i in 1:100){
  predB_x<-jackPredLog(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predB_x)
  ROC_B_log[[i]]<-ROC
}

ROC_C_log<-list() #Model C PS_PR+H + ICS_PR
for(i in 1:100){
  predC_x<-jackPredLog(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predC_x)
  ROC_C_log[[i]]<-ROC
}

ROC_D_log<-list() #Model D: ICS_PR + CD_PR+TE
for(i in 1:100){
  predD_x<-jackPredLog(centAll,predictors = c("gender","ICS_PageRank","CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predD_x)
  ROC_D_log[[i]]<-ROC
}

ROC_E_log<-list() #Model E: ICS_PR + CD_PR
for(i in 1:100){
  predE_x<-jackPredLog(centAll,predictors = c("gender","ICS_PageRank","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predE_x)
  ROC_E_log[[i]]<-ROC
}

ROC_F_log<-list() #Model F: PS_PR+H + ICS_PR+H + CD_PR
for(i in 1:100){
  predF_x<-jackPredLog(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","ICS_Hide","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predF_x)
  ROC_F_log[[i]]<-ROC
}

ROC_G_log<-list() #Model B: PS_PR+H + ICS_PR+H + CD_PR+TE
for(i in 1:100){
  predG_x<-jackPredLog(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","ICS_Hide","CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predG_x)
  ROC_G_log[[i]]<-ROC
}

ROC_H_log<-list() #Model C PS_PR+H + ICS_PR
for(i in 1:100){
  predH_x<-jackPredLog(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","ICS_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predH_x)
  ROC_H_log[[i]]<-ROC
}

ROC_I_log<-list() #Model D: ICS_PR+H + CD_PR+TE
for(i in 1:100){
  predI_x<-jackPredLog(centAll,predictors = c("gender","ICS_PageRank","ICS_Hide","CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predI_x)
  ROC_I_log[[i]]<-ROC
}

ROC_J_log<-list() #Model J: ICS_PR+H + CD_PR
for(i in 1:100){
  predJ_x<-jackPredLog(centAll,predictors = c("gender","ICS_PageRank","ICS_Hide","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predJ_x)
  ROC_J_log[[i]]<-ROC
}
######BEST NETWORK PREDICTORS JUST PASSED######
ROC_K_log<-list() #Model K: ICS_PR + CD_PR+H+TE
for(i in 1:100){
  predK_x<-jackPredLog(centAll,outcome = "justpass",predictors = c("gender","ICS_PageRank","CD_PageRank","CD_tarEnt","CD_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predK_x)
  ROC_K_log[[i]]<-ROC
}

ROC_L_log<-list() #Model K: ICS_PR + CD_PR+H
for(i in 1:100){
  predL_x<-jackPredLog(centAll,outcome = "justpass",predictors = c("gender","ICS_PageRank","CD_PageRank","CD_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predL_x)
  ROC_L_log[[i]]<-ROC
}

ROC_M_log<-list() #Model K: ICS_PR + CD_PR
for(i in 1:100){
  predM_x<-jackPredLog(centAll,outcome = "justpass",predictors = c("gender","ICS_PageRank","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predM_x)
  ROC_M_log[[i]]<-ROC
}

ROC_N_log<-list() #Model K: ICS_PR + CD_H
for(i in 1:100){
  predN_x<-jackPredLog(centAll,outcome = "justpass",predictors = c("gender","ICS_PageRank","CD_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predN_x)
  ROC_N_log[[i]]<-ROC
}
#
save(ROC_A_log,ROC_B_log,ROC_C_log,ROC_D_log,ROC_E_log,ROC_F_log,ROC_G_log,ROC_H_log,ROC_I_log
     ,ROC_J_log,ROC_K_log,ROC_L_log,ROC_M_log,ROC_N_log,file="data/ROC_AUC/ROC13_MixLayerGender_logreg.Rdata")

tend<-Sys.time()
tend-tstart

t1<-Sys.time()
#########LINEAR DISCRIMINANT ANALYSIS###########
#########LINEAR DISCRIMINANT ANALYSIS###########
#########LINEAR DISCRIMINANT ANALYSIS ###########
#########LINEAR DISCRIMINANT ANALYSIS ###########
######BEST NETWORK PREDICTORS PASSED######

ROC_A_lda<-list() #Model A: PS_PR+H + ICS_PR + CD_PR
for(i in 1:100){
  predA_x<-jackPredLDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predA_x)
  ROC_A_lda[[i]]<-ROC
}

ROC_B_lda<-list() #Model B: PS_PR+H + ICS_PR + CD_PR+TE
for(i in 1:100){
  predB_x<-jackPredLDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predB_x)
  ROC_B_lda[[i]]<-ROC
}

ROC_C_lda<-list() #Model C PS_PR+H + ICS_PR
for(i in 1:100){
  predC_x<-jackPredLDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predC_x)
  ROC_C_lda[[i]]<-ROC
}

ROC_D_lda<-list() #Model D: ICS_PR + CD_PR+TE
for(i in 1:100){
  predD_x<-jackPredLDA(centAll,predictors = c("gender","ICS_PageRank","CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predD_x)
  ROC_D_lda[[i]]<-ROC
}

ROC_E_lda<-list() #Model E: ICS_PR + CD_PR
for(i in 1:100){
  predE_x<-jackPredLDA(centAll,predictors = c("gender","ICS_PageRank","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predE_x)
  ROC_E_lda[[i]]<-ROC
}

ROC_F_lda<-list() #Model F: PS_PR+H + ICS_PR+H + CD_PR
for(i in 1:100){
  predF_x<-jackPredLDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","ICS_Hide","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predF_x)
  ROC_F_lda[[i]]<-ROC
}

ROC_G_lda<-list() #Model B: PS_PR+H + ICS_PR+H + CD_PR+TE
for(i in 1:100){
  predG_x<-jackPredLDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","ICS_Hide","CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predG_x)
  ROC_G_lda[[i]]<-ROC
}

ROC_H_lda<-list() #Model C PS_PR+H + ICS_PR
for(i in 1:100){
  predH_x<-jackPredLDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide","ICS_PageRank","ICS_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predH_x)
  ROC_H_lda[[i]]<-ROC
}

ROC_I_lda<-list() #Model D: ICS_PR+H + CD_PR+TE
for(i in 1:100){
  predI_x<-jackPredLDA(centAll,predictors = c("gender","ICS_PageRank","ICS_Hide","CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predI_x)
  ROC_I_lda[[i]]<-ROC
}

ROC_J_lda<-list() #Model J: ICS_PR+H + CD_PR
for(i in 1:100){
  predJ_x<-jackPredLDA(centAll,predictors = c("gender","ICS_PageRank","ICS_Hide","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predJ_x)
  ROC_J_lda[[i]]<-ROC
}
######BEST NETWORK PREDICTORS JUST PASSED######
ROC_K_lda<-list() #Model K: ICS_PR + CD_PR+H+TE
for(i in 1:100){
  predK_x<-jackPredLDA(centAll,outcome = "justpass",predictors = c("gender","ICS_PageRank","CD_PageRank","CD_tarEnt","CD_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predK_x)
  ROC_K_lda[[i]]<-ROC
}

ROC_L_lda<-list() #Model K: ICS_PR + CD_PR+H
for(i in 1:100){
  predL_x<-jackPredLDA(centAll,outcome = "justpass",predictors = c("gender","ICS_PageRank","CD_PageRank","CD_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predL_x)
  ROC_L_lda[[i]]<-ROC
}

ROC_M_lda<-list() #Model K: ICS_PR + CD_PR
for(i in 1:100){
  predM_x<-jackPredLDA(centAll,outcome = "justpass",predictors = c("gender","ICS_PageRank","CD_PageRank"),p=i/100)
  ROC<-ROCplusWeeks(predM_x)
  ROC_M_lda[[i]]<-ROC
}

ROC_N_lda<-list() #Model K: ICS_PR + CD_H
for(i in 1:100){
  predN_x<-jackPredLDA(centAll,outcome = "justpass",predictors = c("gender","ICS_PageRank","CD_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predN_x)
  ROC_N_lda[[i]]<-ROC
}
#
save(ROC_A_lda,ROC_B_lda,ROC_C_lda,ROC_D_lda,ROC_E_lda,ROC_F_lda,ROC_G_lda,ROC_H_lda,ROC_I_lda
     ,ROC_J_lda,ROC_K_lda,ROC_L_lda,ROC_M_lda,ROC_N_lda,file="data/ROC_AUC/ROC13_MixLayerGender_lda.Rdata")




#########QUADRATIC DISCRIMINANT ANALYSIS###########
#########QUADRATIC DISCRIMINANT ANALYSIS###########
#########QUADRATIC DISCRIMINANT ANALYSIS ###########
#########QUADRATIC DISCRIMINANT ANALYSIS ###########


######BEST NETWORK PREDICTORS PASSED######

ROC_A_qda<-list() #Model A: PS_PR+TE+H + CD_PR+TE + ICS_PR+TE
for(i in 1:100){
  predA_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt","PS_Hide",
                                              "CD_PageRank","CD_tarEnt",
                                              "ICS_PageRank","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predA_x)
  ROC_A_qda[[i]]<-ROC
}

ROC_B_qda<-list() #Model B: PS_PR+TE+H + CD_PR+TE + ICS_H+TE
for(i in 1:100){
  predB_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt","PS_Hide",
                                              "CD_PageRank","CD_tarEnt",
                                              "ICS_Hide","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predB_x)
  ROC_B_qda[[i]]<-ROC
}

ROC_C_qda<-list() #Model C: PS_PR+TE+H + CD_PR+TE + ICS_PR+TE
for(i in 1:100){
  predC_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt","PS_Hide",
                                              "CD_PageRank","CD_tarEnt",
                                              "ICS_PageRank","ICS_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predC_x)
  ROC_C_qda[[i]]<-ROC
}

ROC_D_qda<-list() #Model A: PS_PR+TE+H + CD_TE+H + ICS_PR+TE
for(i in 1:100){
  predD_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt","PS_Hide",
                                              "CD_Hide","CD_tarEnt",
                                              "ICS_PageRank","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predD_x)
  ROC_D_qda[[i]]<-ROC
}

ROC_E_qda<-list() #Model B: PS_PR+TE+H + CD_TE+H + ICS_H+TE
for(i in 1:100){
  predE_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt","PS_Hide",
                                              "CD_Hide","CD_tarEnt",
                                              "ICS_Hide","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predE_x)
  ROC_E_qda[[i]]<-ROC
}

ROC_F_qda<-list() #Model C: PS_PR+TE+H + CD_TE+H + ICS_PR+TE
for(i in 1:100){
  predF_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt","PS_Hide",
                                              "CD_Hide","CD_tarEnt",
                                              "ICS_PageRank","ICS_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predF_x)
  ROC_F_qda[[i]]<-ROC
}

ROC_G_qda<-list() #Model A: CD_PR+TE + ICS_PR+TE
for(i in 1:100){
  predG_x<-jackPredQDA(centAll,predictors = c("gender","CD_PageRank","CD_tarEnt",
                                              "ICS_PageRank","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predG_x)
  ROC_G_qda[[i]]<-ROC
}

ROC_H_qda<-list() #Model H: CD_PR+TE + ICS_H+TE
for(i in 1:100){
  predH_x<-jackPredQDA(centAll,predictors = c("gender","CD_PageRank","CD_tarEnt",
                                              "ICS_Hide","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predH_x)
  ROC_H_qda[[i]]<-ROC
}

ROC_I_qda<-list() #Model I:CD_PR+TE + ICS_PR+TE
for(i in 1:100){
  predI_x<-jackPredQDA(centAll,predictors = c("gender","CD_PageRank","CD_tarEnt",
                                              "ICS_PageRank","ICS_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predI_x)
  ROC_I_qda[[i]]<-ROC
}

ROC_J_qda<-list() #Model J:CD_TE+H + ICS_PR+TE
for(i in 1:100){
  predJ_x<-jackPredQDA(centAll,predictors = c("gender","CD_Hide","CD_tarEnt",
                                              "ICS_PageRank","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predJ_x)
  ROC_J_qda[[i]]<-ROC
}

ROC_K_qda<-list() #Model K: CD_TE+H + ICS_H+TE
for(i in 1:100){
  predK_x<-jackPredQDA(centAll,predictors = c("gender","CD_Hide","CD_tarEnt",
                                              "ICS_Hide","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predK_x)
  ROC_K_qda[[i]]<-ROC
}

ROC_L_qda<-list() #Model L: CD_TE+H + ICS_PR+TE
for(i in 1:100){
  predL_x<-jackPredQDA(centAll,predictors = c("gender","CD_Hide","CD_tarEnt",
                                              "ICS_PageRank","ICS_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predL_x)
  ROC_L_qda[[i]]<-ROC
}

ROC_M_qda<-list() #Model M: PS_PR+TE+H + CD_PR+TE 
for(i in 1:100){
  predM_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt","PS_Hide",
                                              "CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predM_x)
  ROC_M_qda[[i]]<-ROC
}

ROC_N_qda<-list() #Model M: PS_PR+TE+H + CD_TE+H 
for(i in 1:100){
  predN_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt","PS_Hide",
                                              "CD_tarEnt","CD_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predN_x)
  ROC_N_qda[[i]]<-ROC
}

ROC_O_qda<-list() #Model A: PS_PR+TE+H + CD_PR+TE + ICS_PR+TE
for(i in 1:100){
  predO_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide",
                                              "CD_PageRank","CD_tarEnt",
                                              "ICS_PageRank","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predO_x)
  ROC_O_qda[[i]]<-ROC
}

ROC_P_qda<-list() #Model B: PS_PR+TE+H + CD_PR+TE + ICS_H+TE
for(i in 1:100){
  predP_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide",
                                              "CD_PageRank","CD_tarEnt",
                                              "ICS_Hide","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predP_x)
  ROC_P_qda[[i]]<-ROC
}

ROC_Q_qda<-list() #Model C: PS_PR+TE+H + CD_PR+TE + ICS_PR+TE
for(i in 1:100){
  predQ_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide",
                                              "CD_PageRank","CD_tarEnt",
                                              "ICS_PageRank","ICS_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predQ_x)
  ROC_Q_qda[[i]]<-ROC
}

ROC_R_qda<-list() #Model A: PS_PR+TE+H + CD_TE+H + ICS_PR+TE
for(i in 1:100){
  predR_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide",
                                              "CD_Hide","CD_tarEnt",
                                              "ICS_PageRank","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predR_x)
  ROC_R_qda[[i]]<-ROC
}

ROC_S_qda<-list() #Model B: PS_PR+TE+H + CD_TE+H + ICS_H+TE
for(i in 1:100){
  predS_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide",
                                              "CD_Hide","CD_tarEnt",
                                              "ICS_Hide","ICS_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predS_x)
  ROC_S_qda[[i]]<-ROC
}

ROC_T_qda<-list() #Model C: PS_PR+TE+H + CD_TE+H + ICS_PR+TE
for(i in 1:100){
  predT_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide",
                                              "CD_Hide","CD_tarEnt",
                                              "ICS_PageRank","ICS_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predT_x)
  ROC_T_qda[[i]]<-ROC
}

ROC_U_qda<-list() #Model M: PS_PR+TE+H + CD_PR+TE 
for(i in 1:100){
  predU_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide",
                                              "CD_PageRank","CD_tarEnt"),p=i/100)
  ROC<-ROCplusWeeks(predU_x)
  ROC_U_qda[[i]]<-ROC
}

ROC_V_qda<-list() #Model M: PS_PR+TE+H + CD_TE+H 
for(i in 1:100){
  predV_x<-jackPredQDA(centAll,predictors = c("gender","PS_PageRank","PS_Hide",
                                              "CD_tarEnt","CD_Hide"),p=i/100)
  ROC<-ROCplusWeeks(predV_x)
  ROC_V_qda[[i]]<-ROC
}

##JUST PASS PREDICTORS

ROC_W_qda<-list() #Model M: PS_PR+H + CD_TE+H 
for(i in 1:100){
  predW_x<-jackPredQDA(centAll,outcome = "justpass", 
                       predictors = c("gender","CD_PageRank","CD_Hide",
                                      "ICS_PageRank","ICS_tarEnt"),p=i/100) #
  ROC<-ROCplusWeeks(predW_x)
  ROC_W_qda[[i]]<-ROC
}

ROC_X_qda<-list() #Model M: PS_PR+H + CD_TE+H 
for(i in 1:100){
  predX_x<-jackPredQDA(centAll,outcome = "justpass", 
                       predictors = c("gender","CD_PageRank","CD_Hide",
                                      "ICS_tarEnt"),p=i/100) #ERROR: rank deficiency in group 1 
  ROC<-ROCplusWeeks(predX_x)
  ROC_X_qda[[i]]<-ROC
}

save(ROC_A_qda,ROC_B_qda,ROC_C_qda,ROC_D_qda,ROC_E_qda,ROC_F_qda,ROC_G_qda,
     ROC_H_qda,ROC_I_qda,ROC_J_qda,ROC_K_qda,ROC_L_qda,ROC_M_qda,ROC_N_qda,
     ROC_O_qda,ROC_P_qda,ROC_Q_qda,ROC_R_qda,ROC_S_qda,ROC_T_qda,ROC_U_qda,
     ROC_V_qda,ROC_W_qda,ROC_X_qda,file="data/ROC_AUC/ROC13_MixLayerGender_qda.Rdata")



#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########
#########K NEAREST NEIGHBORS###########

######BEST NETWORK PREDICTORS PASSED######
ROC_A_knn<-list()
for(i in 1:20){
  predA_x<-jackPredKNN(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt", "PS_Hide",
                                              "CD_PageRank","CD_tarEnt", "CD_Hide",
                                              "ICS_PageRank","ICS_tarEnt","ICS_Hide"),nK = i)
  ROC<-ROCplusWeeks(predA_x$allpred)
  ROC_A_knn[[i]]<-ROC
}

ROC_B_knn<-list()
for(i in 1:20){
  predB_x<-jackPredKNN(centAll,predictors = c("gender","CD_PageRank","CD_tarEnt", "CD_Hide",
                                              "ICS_PageRank","ICS_tarEnt","ICS_Hide"),nK = i)
  ROC<-ROCplusWeeks(predB_x$allpred)
  ROC_B_knn[[i]]<-ROC
}

ROC_C_knn<-list()
for(i in 1:20){
  predC_x<-jackPredKNN(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt", "PS_Hide",
                                              "CD_PageRank","CD_tarEnt", "CD_Hide"),nK = i)
  ROC<-ROCplusWeeks(predC_x$allpred)
  ROC_C_knn[[i]]<-ROC
}

ROC_D_knn<-list()
for(i in 1:20){
  predD_x<-jackPredKNN(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt", "PS_Hide",
                                              "ICS_PageRank","ICS_tarEnt","ICS_Hide"),nK = i)
  ROC<-ROCplusWeeks(predD_x$allpred)
  ROC_D_knn[[i]]<-ROC
}

######BEST NETWORK PREDICTORS JUST PASSED######
ROC_E_knn<-list()
for(i in 1:20){
  predE_x<-jackPredKNN(centAll,outcome = "justpass",predictors = c("gender","PS_PageRank","PS_tarEnt", "PS_Hide",
                                                                   "CD_PageRank","CD_tarEnt", "CD_Hide",
                                                                   "ICS_PageRank","ICS_tarEnt","ICS_Hide"),nK = i)
  ROC<-ROCplusWeeks(predE_x$allpred)
  ROC_E_knn[[i]]<-ROC
}

ROC_F_knn<-list()
for(i in 1:20){
  predF_x<-jackPredKNN(centAll,predictors = c("gender","CD_PageRank","CD_tarEnt", "CD_Hide",
                                              "ICS_PageRank","ICS_tarEnt","ICS_Hide"),nK = i)
  ROC<-ROCplusWeeks(predF_x$allpred)
  ROC_F_knn[[i]]<-ROC
}

ROC_G_knn<-list()
for(i in 1:20){
  predG_x<-jackPredKNN(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt", "PS_Hide",
                                              "CD_PageRank","CD_tarEnt", "CD_Hide"),nK = i)
  ROC<-ROCplusWeeks(predG_x$allpred)
  ROC_G_knn[[i]]<-ROC
}

ROC_H_knn<-list()
for(i in 1:20){
  predH_x<-jackPredKNN(centAll,predictors = c("gender","PS_PageRank","PS_tarEnt", "PS_Hide",
                                              "ICS_PageRank","ICS_tarEnt","ICS_Hide"),nK = i)
  ROC<-ROCplusWeeks(predH_x$allpred)
  ROC_H_knn[[i]]<-ROC
}
save(ROC_A_knn,ROC_B_knn,ROC_C_knn,ROC_D_knn,
     ROC_E_knn,ROC_F_knn,ROC_G_knn,ROC_H_knn,
     file="data/ROC_AUC/ROC13_MixLayerGender_knn.Rdata")


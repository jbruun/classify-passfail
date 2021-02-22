#######################################################
###LOAD CALCULATIONS FROM log reg, lda, qdua and KNN###
#######################################################
load("data/ROC_AUC/ROC11_NB_logreg.Rdata")
load("data/ROC_AUC/ROC11_NB_lda.Rdata")
load("data/ROC_AUC/ROC11_NB_qda.Rdata")
load("data/ROC_AUC/ROC11_NB_knn.Rdata")

##########OUTCOME = PASSED#########
##########LOGREG#########

ROC_A_log_pfm<-performanceMeasures(ROC_A_log)
A_AUC_NB_log_all<-AUC(ROC_A_log_pfm[[1]],ROC_A_log_pfm[[2]])

ROC_B_log_pfm<-performanceMeasures(ROC_B_log)
B_AUC_NB_log_all<-AUC(ROC_B_log_pfm[[1]],ROC_B_log_pfm[[2]])

ROC_C_log_pfm<-performanceMeasures(ROC_C_log)
C_AUC_NB_log_all<-AUC(ROC_C_log_pfm[[1]],ROC_C_log_pfm[[2]])

ROC_D_log_pfm<-performanceMeasures(ROC_D_log)
D_AUC_NB_log_all<-AUC(ROC_D_log_pfm[[1]],ROC_D_log_pfm[[2]])

ROC_E_log_pfm<-performanceMeasures(ROC_E_log)
E_AUC_NB_log_all<-AUC(ROC_E_log_pfm[[1]],ROC_E_log_pfm[[2]])

ROC_F_log_pfm<-performanceMeasures(ROC_F_log)
F_AUC_NB_log_all<-AUC(ROC_F_log_pfm[[1]],ROC_F_log_pfm[[2]])

ROC_G_log_pfm<-performanceMeasures(ROC_G_log)
G_AUC_NB_log_all<-AUC(ROC_G_log_pfm[[1]],ROC_G_log_pfm[[2]])

ROC_H_log_pfm<-performanceMeasures(ROC_H_log)
H_AUC_NB_log_all<-AUC(ROC_H_log_pfm[[1]],ROC_H_log_pfm[[2]])

ROC_I_log_pfm<-performanceMeasures(ROC_I_log)
I_AUC_NB_log_all<-AUC(ROC_I_log_pfm[[1]],ROC_I_log_pfm[[2]])

ROC_J_log_pfm<-performanceMeasures(ROC_J_log)
J_AUC_NB_log_all<-AUC(ROC_J_log_pfm[[1]],ROC_J_log_pfm[[2]])

###LOG REG JUST PASSED

ROC_K_log_pfm<-performanceMeasures(ROC_K_log)
K_AUC_NB_log_all<-AUC(ROC_K_log_pfm[[1]],ROC_K_log_pfm[[2]])

ROC_L_log_pfm<-performanceMeasures(ROC_L_log)
L_AUC_NB_log_all<-AUC(ROC_L_log_pfm[[1]],ROC_L_log_pfm[[2]])

ROC_M_log_pfm<-performanceMeasures(ROC_M_log)
M_AUC_NB_log_all<-AUC(ROC_M_log_pfm[[1]],ROC_M_log_pfm[[2]])

ROC_N_log_pfm<-performanceMeasures(ROC_N_log)
N_AUC_NB_log_all<-AUC(ROC_N_log_pfm[[1]],ROC_N_log_pfm[[2]])

#########LDA#########
###### PASSED######
ROC_A_lda_pfm<-performanceMeasures(ROC_A_lda)
A_AUC_NB_lda_all<-AUC(ROC_A_lda_pfm[[1]],ROC_A_lda_pfm[[2]])

ROC_B_lda_pfm<-performanceMeasures(ROC_B_lda)
B_AUC_NB_lda_all<-AUC(ROC_B_lda_pfm[[1]],ROC_B_lda_pfm[[2]])

ROC_C_lda_pfm<-performanceMeasures(ROC_C_lda)
C_AUC_NB_lda_all<-AUC(ROC_C_lda_pfm[[1]],ROC_C_lda_pfm[[2]])

ROC_D_lda_pfm<-performanceMeasures(ROC_D_lda)
D_AUC_NB_lda_all<-AUC(ROC_D_lda_pfm[[1]],ROC_D_lda_pfm[[2]])

ROC_E_lda_pfm<-performanceMeasures(ROC_E_lda)
E_AUC_NB_lda_all<-AUC(ROC_E_lda_pfm[[1]],ROC_E_lda_pfm[[2]])

ROC_F_lda_pfm<-performanceMeasures(ROC_F_lda)
F_AUC_NB_lda_all<-AUC(ROC_F_lda_pfm[[1]],ROC_F_lda_pfm[[2]])

ROC_G_lda_pfm<-performanceMeasures(ROC_G_lda)
G_AUC_NB_lda_all<-AUC(ROC_G_lda_pfm[[1]],ROC_G_lda_pfm[[2]])

ROC_H_lda_pfm<-performanceMeasures(ROC_H_lda)
H_AUC_NB_lda_all<-AUC(ROC_H_lda_pfm[[1]],ROC_H_lda_pfm[[2]])

ROC_I_lda_pfm<-performanceMeasures(ROC_I_lda)
I_AUC_NB_lda_all<-AUC(ROC_I_lda_pfm[[1]],ROC_I_lda_pfm[[2]])

ROC_J_lda_pfm<-performanceMeasures(ROC_J_lda)
J_AUC_NB_lda_all<-AUC(ROC_J_lda_pfm[[1]],ROC_J_lda_pfm[[2]])
######JUST PASSED######

ROC_K_lda_pfm<-performanceMeasures(ROC_K_lda)
K_AUC_NB_lda_all<-AUC(ROC_K_lda_pfm[[1]],ROC_K_lda_pfm[[2]])

ROC_L_lda_pfm<-performanceMeasures(ROC_L_lda)
L_AUC_NB_lda_all<-AUC(ROC_L_lda_pfm[[1]],ROC_L_lda_pfm[[2]])

ROC_M_lda_pfm<-performanceMeasures(ROC_M_lda)
M_AUC_NB_lda_all<-AUC(ROC_M_lda_pfm[[1]],ROC_M_lda_pfm[[2]])

ROC_N_lda_pfm<-performanceMeasures(ROC_N_lda)
N_AUC_NB_lda_all<-AUC(ROC_N_lda_pfm[[1]],ROC_N_lda_pfm[[2]])


##########QDA#########
######PASSED######
ROC_A_qda_pfm<-performanceMeasures(ROC_A_qda)
A_AUC_NB_qda_all<-AUC(ROC_A_qda_pfm[[1]],ROC_A_qda_pfm[[2]])

ROC_B_qda_pfm<-performanceMeasures(ROC_B_qda)
B_AUC_NB_qda_all<-AUC(ROC_B_qda_pfm[[1]],ROC_B_qda_pfm[[2]])

ROC_C_qda_pfm<-performanceMeasures(ROC_C_qda)
C_AUC_NB_qda_all<-AUC(ROC_C_qda_pfm[[1]],ROC_C_qda_pfm[[2]])

ROC_D_qda_pfm<-performanceMeasures(ROC_D_qda)
D_AUC_NB_qda_all<-AUC(ROC_D_qda_pfm[[1]],ROC_D_qda_pfm[[2]])

ROC_E_qda_pfm<-performanceMeasures(ROC_E_qda)
E_AUC_NB_qda_all<-AUC(ROC_E_qda_pfm[[1]],ROC_E_qda_pfm[[2]])

ROC_F_qda_pfm<-performanceMeasures(ROC_F_qda)
F_AUC_NB_qda_all<-AUC(ROC_F_qda_pfm[[1]],ROC_F_qda_pfm[[2]])

ROC_G_qda_pfm<-performanceMeasures(ROC_G_qda)
G_AUC_NB_qda_all<-AUC(ROC_G_qda_pfm[[1]],ROC_G_qda_pfm[[2]])

ROC_H_qda_pfm<-performanceMeasures(ROC_H_qda)
H_AUC_NB_qda_all<-AUC(ROC_H_qda_pfm[[1]],ROC_H_qda_pfm[[2]])

ROC_I_qda_pfm<-performanceMeasures(ROC_I_qda)
I_AUC_NB_qda_all<-AUC(ROC_I_qda_pfm[[1]],ROC_I_qda_pfm[[2]])

ROC_J_qda_pfm<-performanceMeasures(ROC_J_qda)
J_AUC_NB_qda_all<-AUC(ROC_J_qda_pfm[[1]],ROC_J_qda_pfm[[2]])

ROC_K_qda_pfm<-performanceMeasures(ROC_K_qda)
K_AUC_NB_qda_all<-AUC(ROC_K_qda_pfm[[1]],ROC_K_qda_pfm[[2]])

ROC_L_qda_pfm<-performanceMeasures(ROC_L_qda)
L_AUC_NB_qda_all<-AUC(ROC_L_qda_pfm[[1]],ROC_L_qda_pfm[[2]])

ROC_M_qda_pfm<-performanceMeasures(ROC_M_qda)
M_AUC_NB_qda_all<-AUC(ROC_M_qda_pfm[[1]],ROC_M_qda_pfm[[2]])

ROC_N_qda_pfm<-performanceMeasures(ROC_N_qda)
N_AUC_NB_qda_all<-AUC(ROC_N_qda_pfm[[1]],ROC_N_qda_pfm[[2]])

ROC_O_qda_pfm<-performanceMeasures(ROC_O_qda)
O_AUC_NB_qda_all<-AUC(ROC_O_qda_pfm[[1]],ROC_O_qda_pfm[[2]])

ROC_P_qda_pfm<-performanceMeasures(ROC_P_qda)
P_AUC_NB_qda_all<-AUC(ROC_P_qda_pfm[[1]],ROC_P_qda_pfm[[2]])

ROC_Q_qda_pfm<-performanceMeasures(ROC_Q_qda)
Q_AUC_NB_qda_all<-AUC(ROC_Q_qda_pfm[[1]],ROC_Q_qda_pfm[[2]])

ROC_R_qda_pfm<-performanceMeasures(ROC_R_qda)
R_AUC_NB_qda_all<-AUC(ROC_R_qda_pfm[[1]],ROC_R_qda_pfm[[2]])

ROC_S_qda_pfm<-performanceMeasures(ROC_S_qda)
S_AUC_NB_qda_all<-AUC(ROC_S_qda_pfm[[1]],ROC_S_qda_pfm[[2]])

ROC_T_qda_pfm<-performanceMeasures(ROC_T_qda)
T_AUC_NB_qda_all<-AUC(ROC_T_qda_pfm[[1]],ROC_T_qda_pfm[[2]])

ROC_U_qda_pfm<-performanceMeasures(ROC_U_qda)
U_AUC_NB_qda_all<-AUC(ROC_U_qda_pfm[[1]],ROC_U_qda_pfm[[2]])

ROC_V_qda_pfm<-performanceMeasures(ROC_V_qda)
V_AUC_NB_qda_all<-AUC(ROC_V_qda_pfm[[1]],ROC_V_qda_pfm[[2]])
######JUST PASSED######

ROC_W_qda_pfm<-performanceMeasures(ROC_W_qda)
W_AUC_NB_qda_all<-AUC(ROC_W_qda_pfm[[1]],ROC_W_qda_pfm[[2]])

ROC_X_qda_pfm<-performanceMeasures(ROC_X_qda)
X_AUC_NB_qda_all<-AUC(ROC_X_qda_pfm[[1]],ROC_X_qda_pfm[[2]])

##########KNN#########
###PASSED#####
ROC_A_log_pfm<-performanceMeasures(ROC_A_log)
A_AUC_NB_log_all<-AUC(ROC_A_log_pfm[[1]],ROC_A_log_pfm[[2]])

ROC_B_log_pfm<-performanceMeasures(ROC_B_log)
B_AUC_NB_log_all<-AUC(ROC_B_log_pfm[[1]],ROC_B_log_pfm[[2]])

ROC_C_log_pfm<-performanceMeasures(ROC_C_log)
C_AUC_NB_log_all<-AUC(ROC_C_log_pfm[[1]],ROC_C_log_pfm[[2]])

ROC_D_log_pfm<-performanceMeasures(ROC_D_log)
D_AUC_NB_log_all<-AUC(ROC_D_log_pfm[[1]],ROC_D_log_pfm[[2]])
###JUST PASSED#####

ROC_E_log_pfm<-performanceMeasures(ROC_E_log)
E_AUC_NB_log_all<-AUC(ROC_E_log_pfm[[1]],ROC_E_log_pfm[[2]])

ROC_F_log_pfm<-performanceMeasures(ROC_F_log)
F_AUC_NB_log_all<-AUC(ROC_F_log_pfm[[1]],ROC_F_log_pfm[[2]])

ROC_G_log_pfm<-performanceMeasures(ROC_G_log)
G_AUC_NB_log_all<-AUC(ROC_G_log_pfm[[1]],ROC_G_log_pfm[[2]])

ROC_H_log_pfm<-performanceMeasures(ROC_H_log)
H_AUC_NB_log_all<-AUC(ROC_H_log_pfm[[1]],ROC_H_log_pfm[[2]])



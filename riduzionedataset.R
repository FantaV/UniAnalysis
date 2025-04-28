library(caret)
library(tidyverse)
library(data.table)

path = "/Users/carlovilloresi/Documents/Coding/R/Projects/7. Data Preparation ONG/"

nome_bigfile <- paste0(path, "dataset_ong.txt")
big_dataset <- fread(nome_bigfile, sep=",", header=T, nThread = 4, data.table=F)

supporter_id <- big_dataset$Supporter_ID
target <- big_dataset$TARGET_UPSELL
am <- big_dataset$AM

var_num <- c(#"AM",
             #"Supporter_ID",
             #"TARGET_UPSELL",
             "TENURE_AM_TG",
             "RITARDI_PAGAMENTI_1A",
             "RITARDI_PAGAMENTI_2A",
             "RITARDI_PAGAMENTI_3A",
             "Dialog_Pos_1A_Sum_Telef_1A_Sum_Prop",
             "Dialog_Neg_1A_Sum_Telef_1A_Sum_Prop",
             "Non_Rep_1A_Sum_Telef_1A_Sum_Prop",
             "Rif_Dial_1A_Sum_Telef_1A_Sum_Prop",
             "DIALOGHI_1A_Sum_Telef_1A_Sum_Prop",
             "Dialog_Pos_2A_Sum_Telef_2A_Sum_Prop",
             "Dialog_Neg_2A_Sum_Telef_2A_Sum_Prop",
             "Non_Rep_2A_Sum_Telef_2A_Sum_Prop",
             "Rif_Dial_2A_Sum_Telef_2A_Sum_Prop",
             "DIALOGHI_2A_Sum_Telef_2A_Sum_Prop",
             "Dialog_Pos_3A_Sum_Telef_3A_Sum_Prop",
             "Dialog_Neg_3A_Sum_Telef_3A_Sum_Prop",
             "Non_Rep_3A_Sum_Telef_3A_Sum_Prop",
             "Rif_Dial_3A_Sum_Telef_3A_Sum_Prop",
             "DIALOGHI_3A_Sum_Telef_3A_Sum_Prop",
             "Dialog_Pos_4A_Sum_Telef_4A_Sum_Prop",
             "Dialog_Neg_4A_Sum_Telef_4A_Sum_Prop",
             "Non_Rep_4A_Sum_Telef_4A_Sum_Prop",
             "Rif_Dial_4A_Sum_Telef_4A_Sum_Prop",
             "DIALOGHI_4A_Sum_Telef_4A_Sum_Prop",
             "OPEN_UNSUB_1S_Sum_RICEVUTE_1S_Sum",
             "OPEN_CLICK_UNSUB_1S_Sum_RICEVUTE_1S_Sum",
             "OPEN_CLICK_1S_Sum_RICEVUTE_1S_Sum",
             "NESSUNA_ATTIVITA_1S_Sum_RICEVUTE_1S_Sum",
             "OPEN_1S_Sum_RICEVUTE_1S_Sum",
             "OPEN_UNSUB_2S_Sum_RICEVUTE_2S_Sum",
             "OPEN_CLICK_UNSUB_2S_Sum_RICEVUTE_2S_Sum",
             "OPEN_CLICK_2S_Sum_RICEVUTE_2S_Sum",
             "NESSUNA_ATTIVITA_2S_Sum_RICEVUTE_2S_Sum",
             "OPEN_2S_Sum_RICEVUTE_2S_Sum",
             "OPEN_UNSUB_3S_Sum_RICEVUTE_3S_Sum",
             "OPEN_CLICK_UNSUB_3S_Sum_RICEVUTE_3S_Sum",
             "OPEN_CLICK_3S_Sum_RICEVUTE_3S_Sum",
             "NESSUNA_ATTIVITA_3S_Sum_RICEVUTE_3S_Sum",
             "OPEN_3S_Sum_RICEVUTE_3S_Sum",
             "OPEN_UNSUB_4S_Sum_RICEVUTE_4S_Sum",
             "OPEN_CLICK_UNSUB_4S_Sum_RICEVUTE_4S_Sum",
             "OPEN_CLICK_4S_Sum_RICEVUTE_4S_Sum",
             "NESSUNA_ATTIVITA_4S_Sum_RICEVUTE_4S_Sum",
             "OPEN_4S_Sum_RICEVUTE_4S_Sum",
             "OPEN_UNSUB_5S_Sum_RICEVUTE_5S_Sum",
             "OPEN_CLICK_UNSUB_5S_Sum_RICEVUTE_5S_Sum",
             "OPEN_CLICK_5S_Sum_RICEVUTE_5S_Sum",
             "NESSUNA_ATTIVITA_5S_Sum_RICEVUTE_5S_Sum",
             "OPEN_5S_Sum_RICEVUTE_5S_Sum",
             "OPEN_UNSUB_6S_Sum_RICEVUTE_6S_Sum",
             "OPEN_CLICK_UNSUB_6S_Sum_RICEVUTE_6S_Sum",
             "OPEN_CLICK_6S_Sum_RICEVUTE_6S_Sum",
             "NESSUNA_ATTIVITA_6S_Sum_RICEVUTE_6S_Sum",
             "OPEN_6S_Sum_RICEVUTE_6S_Sum",
             "Diff_Mesi_2_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_3_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_4_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_5_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_6_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_7_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_8_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_9_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_10_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_11_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_12_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_13_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_14_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_15_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_16_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_17_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_18_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_19_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_20_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_21_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_22_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_23_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_24_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_25_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_26_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_27_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_28_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_29_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_30_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_31_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_32_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_33_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_34_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_35_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_36_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_37_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_38_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_39_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_40_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_41_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_42_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_43_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_44_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_45_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_46_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_47_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_48_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_49_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_50_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_51_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_52_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_53_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_54_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_55_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_56_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_57_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_58_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_59_OO_INST_AMOUNT_MENSILE_Sum",
             "Diff_Mesi_60_OO_INST_AMOUNT_MENSILE_Sum",
             "DAYS_FROM_LAST_CASH_DON",
             "Diff_Mesi_2_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_3_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_4_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_5_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_6_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_7_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_8_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_9_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_10_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_11_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_12_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_13_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_14_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_15_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_16_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_17_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_18_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_19_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_20_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_21_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_22_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_23_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_24_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_25_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_26_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_27_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_28_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_29_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_30_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_31_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_32_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_33_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_34_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_35_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_36_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_37_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_38_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_39_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_40_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_41_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_42_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_43_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_44_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_45_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_46_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_47_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_48_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_49_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_50_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_51_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_52_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_53_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_54_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_55_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_56_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_57_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_58_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_59_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_60_IMPORTO_DONATO_OO_Sum",
             "Diff_Mesi_2_NUM_DON_OO_Sum",
             "Diff_Mesi_3_NUM_DON_OO_Sum",
             "Diff_Mesi_4_NUM_DON_OO_Sum",
             "Diff_Mesi_5_NUM_DON_OO_Sum",
             "Diff_Mesi_6_NUM_DON_OO_Sum",
             "Diff_Mesi_7_NUM_DON_OO_Sum",
             "Diff_Mesi_8_NUM_DON_OO_Sum",
             "Diff_Mesi_9_NUM_DON_OO_Sum",
             "Diff_Mesi_10_NUM_DON_OO_Sum",
             "Diff_Mesi_11_NUM_DON_OO_Sum",
             "Diff_Mesi_12_NUM_DON_OO_Sum",
             "Diff_Mesi_13_NUM_DON_OO_Sum",
             "Diff_Mesi_14_NUM_DON_OO_Sum",
             "Diff_Mesi_15_NUM_DON_OO_Sum",
             "Diff_Mesi_16_NUM_DON_OO_Sum",
             "Diff_Mesi_17_NUM_DON_OO_Sum",
             "Diff_Mesi_18_NUM_DON_OO_Sum",
             "Diff_Mesi_19_NUM_DON_OO_Sum",
             "Diff_Mesi_20_NUM_DON_OO_Sum",
             "Diff_Mesi_21_NUM_DON_OO_Sum",
             "Diff_Mesi_22_NUM_DON_OO_Sum",
             "Diff_Mesi_23_NUM_DON_OO_Sum",
             "Diff_Mesi_24_NUM_DON_OO_Sum",
             "Diff_Mesi_25_NUM_DON_OO_Sum",
             "Diff_Mesi_26_NUM_DON_OO_Sum",
             "Diff_Mesi_27_NUM_DON_OO_Sum",
             "Diff_Mesi_28_NUM_DON_OO_Sum",
             "Diff_Mesi_29_NUM_DON_OO_Sum",
             "Diff_Mesi_30_NUM_DON_OO_Sum",
             "Diff_Mesi_31_NUM_DON_OO_Sum",
             "Diff_Mesi_32_NUM_DON_OO_Sum",
             "Diff_Mesi_33_NUM_DON_OO_Sum",
             "Diff_Mesi_34_NUM_DON_OO_Sum",
             "Diff_Mesi_35_NUM_DON_OO_Sum",
             "Diff_Mesi_36_NUM_DON_OO_Sum",
             "Diff_Mesi_37_NUM_DON_OO_Sum",
             "Diff_Mesi_38_NUM_DON_OO_Sum",
             "Diff_Mesi_39_NUM_DON_OO_Sum",
             "Diff_Mesi_40_NUM_DON_OO_Sum",
             "Diff_Mesi_41_NUM_DON_OO_Sum",
             "Diff_Mesi_42_NUM_DON_OO_Sum",
             "Diff_Mesi_43_NUM_DON_OO_Sum",
             "Diff_Mesi_44_NUM_DON_OO_Sum",
             "Diff_Mesi_45_NUM_DON_OO_Sum",
             "Diff_Mesi_46_NUM_DON_OO_Sum",
             "Diff_Mesi_47_NUM_DON_OO_Sum",
             "Diff_Mesi_48_NUM_DON_OO_Sum",
             "Diff_Mesi_49_NUM_DON_OO_Sum",
             "Diff_Mesi_50_NUM_DON_OO_Sum",
             "Diff_Mesi_51_NUM_DON_OO_Sum",
             "Diff_Mesi_52_NUM_DON_OO_Sum",
             "Diff_Mesi_53_NUM_DON_OO_Sum",
             "Diff_Mesi_54_NUM_DON_OO_Sum",
             "Diff_Mesi_55_NUM_DON_OO_Sum",
             "Diff_Mesi_56_NUM_DON_OO_Sum",
             "Diff_Mesi_57_NUM_DON_OO_Sum",
             "Diff_Mesi_58_NUM_DON_OO_Sum",
             "Diff_Mesi_59_NUM_DON_OO_Sum",
             "Diff_Mesi_60_NUM_DON_OO_Sum",
             "Diff_Mesi_2_UPGRADE_SP_Sum",
             "Diff_Mesi_3_UPGRADE_SP_Sum",
             "Diff_Mesi_4_UPGRADE_SP_Sum",
             "Diff_Mesi_5_UPGRADE_SP_Sum",
             "Diff_Mesi_6_UPGRADE_SP_Sum",
             "Diff_Mesi_7_UPGRADE_SP_Sum",
             "Diff_Mesi_8_UPGRADE_SP_Sum",
             "Diff_Mesi_9_UPGRADE_SP_Sum",
             "Diff_Mesi_10_UPGRADE_SP_Sum",
             "Diff_Mesi_11_UPGRADE_SP_Sum",
             "Diff_Mesi_12_UPGRADE_SP_Sum",
             "Diff_Mesi_13_UPGRADE_SP_Sum",
             "Diff_Mesi_14_UPGRADE_SP_Sum",
             "Diff_Mesi_15_UPGRADE_SP_Sum",
             "Diff_Mesi_16_UPGRADE_SP_Sum",
             "Diff_Mesi_17_UPGRADE_SP_Sum",
             "Diff_Mesi_18_UPGRADE_SP_Sum",
             "Diff_Mesi_19_UPGRADE_SP_Sum",
             "Diff_Mesi_20_UPGRADE_SP_Sum",
             "Diff_Mesi_21_UPGRADE_SP_Sum",
             "Diff_Mesi_22_UPGRADE_SP_Sum",
             "Diff_Mesi_23_UPGRADE_SP_Sum",
             "Diff_Mesi_24_UPGRADE_SP_Sum",
             "Diff_Mesi_25_UPGRADE_SP_Sum",
             "Diff_Mesi_26_UPGRADE_SP_Sum",
             "Diff_Mesi_27_UPGRADE_SP_Sum",
             "Diff_Mesi_28_UPGRADE_SP_Sum",
             "Diff_Mesi_29_UPGRADE_SP_Sum",
             "Diff_Mesi_30_UPGRADE_SP_Sum",
             "Diff_Mesi_31_UPGRADE_SP_Sum",
             "Diff_Mesi_32_UPGRADE_SP_Sum",
             "Diff_Mesi_33_UPGRADE_SP_Sum",
             "Diff_Mesi_34_UPGRADE_SP_Sum",
             "Diff_Mesi_35_UPGRADE_SP_Sum",
             "Diff_Mesi_36_UPGRADE_SP_Sum",
             "Diff_Mesi_37_UPGRADE_SP_Sum",
             "Diff_Mesi_38_UPGRADE_SP_Sum",
             "Diff_Mesi_39_UPGRADE_SP_Sum",
             "Diff_Mesi_40_UPGRADE_SP_Sum",
             "Diff_Mesi_41_UPGRADE_SP_Sum",
             "Diff_Mesi_42_UPGRADE_SP_Sum",
             "Diff_Mesi_43_UPGRADE_SP_Sum",
             "Diff_Mesi_44_UPGRADE_SP_Sum",
             "Diff_Mesi_45_UPGRADE_SP_Sum",
             "Diff_Mesi_46_UPGRADE_SP_Sum",
             "Diff_Mesi_47_UPGRADE_SP_Sum",
             "Diff_Mesi_48_UPGRADE_SP_Sum",
             "Diff_Mesi_49_UPGRADE_SP_Sum",
             "Diff_Mesi_50_UPGRADE_SP_Sum",
             "Diff_Mesi_51_UPGRADE_SP_Sum",
             "Diff_Mesi_52_UPGRADE_SP_Sum",
             "Diff_Mesi_53_UPGRADE_SP_Sum",
             "Diff_Mesi_54_UPGRADE_SP_Sum",
             "Diff_Mesi_55_UPGRADE_SP_Sum",
             "Diff_Mesi_56_UPGRADE_SP_Sum",
             "Diff_Mesi_57_UPGRADE_SP_Sum",
             "Diff_Mesi_58_UPGRADE_SP_Sum",
             "Diff_Mesi_59_UPGRADE_SP_Sum",
             "Diff_Mesi_60_UPGRADE_SP_Sum",
             "PREV_UP_SP_TENURE",
             "Diff_Mesi_2_UPGRADE_Sum",
             "Diff_Mesi_3_UPGRADE_Sum",
             "Diff_Mesi_4_UPGRADE_Sum",
             "Diff_Mesi_5_UPGRADE_Sum",
             "Diff_Mesi_6_UPGRADE_Sum",
             "Diff_Mesi_7_UPGRADE_Sum",
             "Diff_Mesi_8_UPGRADE_Sum",
             "Diff_Mesi_9_UPGRADE_Sum",
             "Diff_Mesi_10_UPGRADE_Sum",
             "Diff_Mesi_11_UPGRADE_Sum",
             "Diff_Mesi_12_UPGRADE_Sum",
             "Diff_Mesi_13_UPGRADE_Sum",
             "Diff_Mesi_14_UPGRADE_Sum",
             "Diff_Mesi_15_UPGRADE_Sum",
             "Diff_Mesi_16_UPGRADE_Sum",
             "Diff_Mesi_17_UPGRADE_Sum",
             "Diff_Mesi_18_UPGRADE_Sum",
             "Diff_Mesi_19_UPGRADE_Sum",
             "Diff_Mesi_20_UPGRADE_Sum",
             "Diff_Mesi_21_UPGRADE_Sum",
             "Diff_Mesi_22_UPGRADE_Sum",
             "Diff_Mesi_23_UPGRADE_Sum",
             "Diff_Mesi_24_UPGRADE_Sum",
             "Diff_Mesi_25_UPGRADE_Sum",
             "Diff_Mesi_26_UPGRADE_Sum",
             "Diff_Mesi_27_UPGRADE_Sum",
             "Diff_Mesi_28_UPGRADE_Sum",
             "Diff_Mesi_29_UPGRADE_Sum",
             "Diff_Mesi_30_UPGRADE_Sum",
             "Diff_Mesi_31_UPGRADE_Sum",
             "Diff_Mesi_32_UPGRADE_Sum",
             "Diff_Mesi_33_UPGRADE_Sum",
             "Diff_Mesi_34_UPGRADE_Sum",
             "Diff_Mesi_35_UPGRADE_Sum",
             "Diff_Mesi_36_UPGRADE_Sum",
             "Diff_Mesi_37_UPGRADE_Sum",
             "Diff_Mesi_38_UPGRADE_Sum",
             "Diff_Mesi_39_UPGRADE_Sum",
             "Diff_Mesi_40_UPGRADE_Sum",
             "Diff_Mesi_41_UPGRADE_Sum",
             "Diff_Mesi_42_UPGRADE_Sum",
             "Diff_Mesi_43_UPGRADE_Sum",
             "Diff_Mesi_44_UPGRADE_Sum",
             "Diff_Mesi_45_UPGRADE_Sum",
             "Diff_Mesi_46_UPGRADE_Sum",
             "Diff_Mesi_47_UPGRADE_Sum",
             "Diff_Mesi_48_UPGRADE_Sum",
             "Diff_Mesi_49_UPGRADE_Sum",
             "Diff_Mesi_50_UPGRADE_Sum",
             "Diff_Mesi_51_UPGRADE_Sum",
             "Diff_Mesi_52_UPGRADE_Sum",
             "Diff_Mesi_53_UPGRADE_Sum",
             "Diff_Mesi_54_UPGRADE_Sum",
             "Diff_Mesi_55_UPGRADE_Sum",
             "Diff_Mesi_56_UPGRADE_Sum",
             "Diff_Mesi_57_UPGRADE_Sum",
             "Diff_Mesi_58_UPGRADE_Sum",
             "Diff_Mesi_59_UPGRADE_Sum",
             "Diff_Mesi_60_UPGRADE_Sum",
             "PREV_UP_TENURE",
             "Diff_Mesi_2_DOWNGRADE_Sum",
             "Diff_Mesi_3_DOWNGRADE_Sum",
             "Diff_Mesi_4_DOWNGRADE_Sum",
             "Diff_Mesi_5_DOWNGRADE_Sum",
             "Diff_Mesi_6_DOWNGRADE_Sum",
             "Diff_Mesi_7_DOWNGRADE_Sum",
             "Diff_Mesi_8_DOWNGRADE_Sum",
             "Diff_Mesi_9_DOWNGRADE_Sum",
             "Diff_Mesi_10_DOWNGRADE_Sum",
             "Diff_Mesi_11_DOWNGRADE_Sum",
             "Diff_Mesi_12_DOWNGRADE_Sum",
             "Diff_Mesi_13_DOWNGRADE_Sum",
             "Diff_Mesi_14_DOWNGRADE_Sum",
             "Diff_Mesi_15_DOWNGRADE_Sum",
             "Diff_Mesi_16_DOWNGRADE_Sum",
             "Diff_Mesi_17_DOWNGRADE_Sum",
             "Diff_Mesi_18_DOWNGRADE_Sum",
             "Diff_Mesi_19_DOWNGRADE_Sum",
             "Diff_Mesi_20_DOWNGRADE_Sum",
             "Diff_Mesi_21_DOWNGRADE_Sum",
             "Diff_Mesi_22_DOWNGRADE_Sum",
             "Diff_Mesi_23_DOWNGRADE_Sum",
             "Diff_Mesi_24_DOWNGRADE_Sum",
             "Diff_Mesi_25_DOWNGRADE_Sum",
             "Diff_Mesi_26_DOWNGRADE_Sum",
             "Diff_Mesi_27_DOWNGRADE_Sum",
             "Diff_Mesi_28_DOWNGRADE_Sum",
             "Diff_Mesi_29_DOWNGRADE_Sum",
             "Diff_Mesi_30_DOWNGRADE_Sum",
             "Diff_Mesi_31_DOWNGRADE_Sum",
             "Diff_Mesi_32_DOWNGRADE_Sum",
             "Diff_Mesi_33_DOWNGRADE_Sum",
             "Diff_Mesi_34_DOWNGRADE_Sum",
             "Diff_Mesi_35_DOWNGRADE_Sum",
             "Diff_Mesi_36_DOWNGRADE_Sum",
             "Diff_Mesi_37_DOWNGRADE_Sum",
             "Diff_Mesi_38_DOWNGRADE_Sum",
             "Diff_Mesi_39_DOWNGRADE_Sum",
             "Diff_Mesi_40_DOWNGRADE_Sum",
             "Diff_Mesi_41_DOWNGRADE_Sum",
             "Diff_Mesi_42_DOWNGRADE_Sum",
             "Diff_Mesi_43_DOWNGRADE_Sum",
             "Diff_Mesi_44_DOWNGRADE_Sum",
             "Diff_Mesi_45_DOWNGRADE_Sum",
             "Diff_Mesi_46_DOWNGRADE_Sum",
             "Diff_Mesi_47_DOWNGRADE_Sum",
             "Diff_Mesi_48_DOWNGRADE_Sum",
             "Diff_Mesi_49_DOWNGRADE_Sum",
             "Diff_Mesi_50_DOWNGRADE_Sum",
             "Diff_Mesi_51_DOWNGRADE_Sum",
             "Diff_Mesi_52_DOWNGRADE_Sum",
             "Diff_Mesi_53_DOWNGRADE_Sum",
             "Diff_Mesi_54_DOWNGRADE_Sum",
             "Diff_Mesi_55_DOWNGRADE_Sum",
             "Diff_Mesi_56_DOWNGRADE_Sum",
             "Diff_Mesi_57_DOWNGRADE_Sum",
             "Diff_Mesi_58_DOWNGRADE_Sum",
             "Diff_Mesi_59_DOWNGRADE_Sum",
             "Diff_Mesi_60_DOWNGRADE_Sum",
             #"PREV_DOWN_TENURE",
             #"Installment_Amount",
             #"Frequency"
             )


# seleziono le variabili indicate dai colleghi per la PCA
dataset_pca <- big_dataset %>% dplyr::select(one_of(var_num))
rm(big_dataset)
gc()

# calcolo le statistiche descrittive 
library(tidyverse)
stat_desc <- big_dataset %>% 
    map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

# riduco le variabili a 380 
# scarto le variabili costanti 
dataset_pca <- dataset_pca %>%
    select_if(function(v) var(v, na.rm=TRUE) != 0) 

# cancello qualche dataset grande, se serve 
gc()

# addestro ACP
# ci mette molto tempo 
t_inizio <- Sys.time()
acp <- princomp(dataset_pca, cor=T)
t_fine <- Sys.time()
print("tempo di esecuzione acp")
print(t_fine - t_inizio)


# salvo l'oggetto
# la variabile path indica dove voglio che 
# venga salvato l'oggetto
path = "/Users/carlovilloresi/Documents/Coding/R/Projects/"
nome_oggetto_acp <- paste0(path, "acp_grande.obj")
save(acp, file=nome_oggetto_acp)
# perchè il file è così grande? 
# perchè dentro c'è il dataset 

# carico l'oggetto acp così non devo aspettare mezz'ora per rigenerarlo
load(nome_oggetto_acp)


# nel dataframe "score" ci sono le coordinate dei punti sugli assi fattoriali 
proiez <- acp$scores
dim(proiez)


# test: me le ricavo moltiplicando i dati originari (scalati) 
# per la matrice dei loadings
# questo prodotto matriciale prende molto tempo 
# pro_test <- scale(dataset_pca) %*% acp$loadings

# c'è anche il metodo predict.princomp
pro_test_1_all <- predict(acp_2, var_df[1,] )

# ispezione risultati
pro_test[1,1:10]
proiez[1,1:10]




# per trasportare l'oggetto acp in maniera efficiente posso cancellare 
# le coordinate sui fattori 
# l'oggetto "alleggerito" continua a fare le previsioni con il metodo predict 
acp_2 <- acp
acp_2$scores <- NULL

# test per verificare che è ancora in grado di fare predict 
test_proiez <- predict(acp_2, dataset_pca)




# salvo l'oggetto alleggerito
nome_oggetto_acp_light <- paste0(path, "acp_ligth.robj")
save(acp_2, file=nome_oggetto_acp_light)

# analisi dei risultati dell'acp 
# le dev std delle componenti principali sono nell'oggetto sdev 
# in acp

# provo a fae uno scree plot
library(factoextra)
fviz_eig(acp_2, ncp = 30)

# esamino gli autovalori
test <- acp$sdev[acp$sdev > 1]





# il dataset non ha NA
# proviamo a vedere se le variabili sono asimmetyriche
# risposta: sì
library(e1071)
analisi_skew <- sapply(dataset_pca, skewness)
hist(analisi_skew)




# codice per svuotare il worspace in maniera selettiva
# svuotare <- ls()[!(ls() %in% c('dataset_pca'))]
# rm(list=svuotare)


# provo a escludere le variabili correlate 
library(caret)
matcor <- cor(dataset_pca)
hc_1 <- findCorrelation(matcor, cutoff=0.5)
dataset_pca_red <- dataset_pca[,-hc_1]

save(dataset_pca_red, file="f:/dataset_pca_red.robj")

save(matcor, file = "f:/matcor.robj")

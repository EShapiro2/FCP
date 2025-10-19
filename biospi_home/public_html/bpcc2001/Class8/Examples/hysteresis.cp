-language(psifcp).
-include(rates).
global(pA(R1) , bA(R2) , transcribeA(R13) , utrA(R3) , degmA(R4) , degpA(R12) , pR(R5) , rbs(R6) , bR(R8) , transcribeR(R14) , utrR(R9) , degmR(R10) , degpR(R11)).


MODULE::=

	activator#A_GENE | repressor#R_GENE | timer#Timer(transcribeA) | 	timer#Timer(transcribeR) | machineries#BASAL_TRANSCRIPTION | 
	machineries#BASAL_TRANSLATION | machineries#RNA_DEGRADATION |
	machineries#PROTEIN_DEGRADATION .





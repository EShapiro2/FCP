-language(psifcp).
-include(rates).
global(bA(R2),bR(R8),utrA(R3),utrR(R9),degmA(R4),degmR(R10), degpA(R12), degpR(R11)).


BASAL_TRANSCRIPTION::= 
    bA ! [] , BASAL_TRANSCRIPTION ;
    bR ! [] , BASAL_TRANSCRIPTION .

BASAL_TRANSLATION::=
    utrA ! [] , BASAL_TRANSLATION ;
    utrR ! [] , BASAL_TRANSLATION .

RNA_DEGRADATION::=
    degmA ! [] , RNA_DEGRADATION ;
    degmR ! [] , RNA_DEGRADATION .

PROTEIN_DEGRADATION::=
    degpA ! [] , PROTEIN_DEGRADATION ;
    degpR ! [] , PROTEIN_DEGRADATION .









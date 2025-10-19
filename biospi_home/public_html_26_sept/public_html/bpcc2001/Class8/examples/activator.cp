-language(psifcp).
-include(rates).
global(pA(R1),bA(R2),transcribeA(R13),utrA(R3),degmA(R4),degpA(R12),pR(R5),rbs(R6)).


A_GENE::=

<< PROMOTED_A + BASAL_A .

PROMOTED_A::=
    pA ? {unbind_A} , ACTIVATED_TRANSCRIPTION_A(unbind_A) .

BASAL_A::=
    bA ? [] , A_GENE | A_RNA .

ACTIVATED_TRANSCRIPTION_A(unbind)::=
    transcribeA ? [] , A_RNA | ACTIVATED_TRANSCRIPTION_A ;
    unbind   ? [] , A_GENE   >> .

A_RNA::=

<< TRANSLATION_A + DEGRADATION_mA .

TRANSLATION_A::=
    utrA ? [] , A_RNA | A_PROTEIN  .

DEGRADATION_mA::=
    degmA ? [] , true >> .


A_PROTEIN::=

<< release_AR(infinite) , unbind_A_pA(R7a) , unbind_A_pR(R7b) . 
                     PROMOTION_AR + BINDING_R + DEGRADATION_A .

PROMOTION_AR::=
    pA ! {unbind_A_pA} , unbind_A_pA ! [] , A_PROTEIN ;
    pR ! {unbind_A_pR} , unbind_A_pR ! [] , A_PROTEIN .

BINDING_R::=
    rbs ! {release_AR} , BOUND_A_PROTEIN .

BOUND_A_PROTEIN::=
    degpA ? [] , release_AR ! [] , true ;
    release_AR ? [] , A_PROTEIN .

DEGRADATION_A::=
    degpA ? [] , true >> .








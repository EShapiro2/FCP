-language(psifcp).
-include(rates).
global(bR(R8),transcribeR(R14),utrR(R9),degmR(R10),degpR(R11),pR(R5),rbs(R6)).

R_GENE::=

<< PROMOTED_R + BASAL_R .

PROMOTED_R::=
    pR ? {unbind_A} , ACTIVATED_TRANSCRIPTION_R(unbind_A) .

BASAL_R::=
    bR ? [] , R_GENE | R_RNA .


ACTIVATED_TRANSCRIPTION_R(unbind)::=
    transcribeR ? [] , R_RNA | ACTIVATED_TRANSCRIPTION_R ;
    unbind   ? [] , R_GENE >> .

R_RNA::= 

<< TRANSLATION_R + DEGRADATION_mR .

  TRANSLATION_R::=
    utrR ? [] , R_RNA | R_PROTEIN  .
	
  DEGRADATION_mR::=
	degmR ? [] , true >> .

R_PROTEIN::= 

<< BINDING_A + DEGRADATION_R .

   BINDING_A::=
      rbs ? {release_A} , BOUND_R_PROTEIN(release_A) .

   BOUND_R_PROTEIN(release)::=
       release ? []  , R_PROTEIN ;
       degpR ? [] , release ! [] , true .

   DEGRADATION_R::=
       degpR ? [] , true >> .






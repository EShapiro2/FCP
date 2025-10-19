-language(psifcp).
global(breakOH(0.1), breakHOH(0.1),breakOO(0.1), breakHH(0.1),breakCO(0.1),breakCOH(0.3),breakCH(0.37)).
%export(Timer,Timer_OH,Timer_OO,Timer_HH,Timer_CO,Timer_COH,Timer_CH).


Timer::= Timer_OH | Timer_OO | Timer_HH | Timer_CO | Timer_CH | Timer_COH | Timer_HOH .


Timer_OH::= breakOH ! [] , Timer_OH .

Timer_HOH::= breakHOH ! [] , Timer_HOH .

Timer_OO::= breakOO ! [] , Timer_OO .

Timer_HH::= breakHH ! [] , Timer_HH .

Timer_CO::= breakCO ! [] , Timer_CO .

Timer_CH::= breakCH ! [] , Timer_CH .

Timer_COH::= breakCOH ! [] , Timer_COH .
:- discontiguous parent/2.
:- discontiguous sex/2.

% 1 поколение
sex("Alexey_Abrosimov",m).
sex("Yaroslav_Shlekin",m).
% 2 поколение
parent("Dmitriy_Abrosimov","Alexey_Abrosimov").
parent("Natalya_Abrosimova","Alexey_Abrosimov").
parent("Alexey_Shlekin","Yaroslav_Shlekin").
parent("Natalya_Abrosimova","Yaroslav_Shlekin").
sex("Dmitriy_Abrosimov",m).
sex("Natalya_Abrosimova",f).
sex("Alexey_Shlekin",m).
% 3 поколение
parent("Alexandr_Abrosimov","Dmitriy_Abrosimov").
parent("Nina_Abrosimova","Dmitriy_Abrosimov").
parent("Alexandr_Abrosimov","Olya_Gladisheva").
parent("Nina_Abrosimova","Olya_Gladisheva").
parent("Elena_Shlekina","Alexey_Shlekin").
parent("Evgeniy_Shlekin","Alexey_Shlekin").
parent("Elena_Shlekina","Anastasia_Shlekina").
parent("Evgeniy_Shlekin","Anastasia_Shlekina").
sex("Elena_Shlekina",f).
sex("Anastasia_Shlekina",f).
sex("Evgeniy_Shlekin",m).
sex("Alexandr_Abrosimov",m).
sex("Nina_Abrosimova",f).
sex("Olya_Gladisheva",f).
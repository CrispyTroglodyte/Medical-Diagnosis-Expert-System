% Dynamic directive
:- dynamic symptom/1, disease/1, transmission/1, has_symptom/2, common_symptom/1, transmitted_by/2.

% Facts

% Symptoms
symptom(fever).
symptom(cough).
symptom(sore_throat).
symptom(headache).
symptom(fatigue).
symptom(chills).
symptom(body_ache).
symptom(shortness_of_breath).
symptom(diarrhea).
symptom(nausea).
symptom(vomiting).
symptom(loss_of_taste).
symptom(loss_of_smell).
symptom(runny_nose).
symptom(congestion).
symptom(rash).
symptom(muscle_pain).
symptom(joint_pain).
symptom(sweating).
symptom(appetite_loss).

% Diseases
disease(covid_19).
disease(flu).
disease(common_cold).
disease(strep_throat).
disease(norovirus).

% Transmission methods
transmission(airborne).
transmission(direct_contact).
transmission(contaminated_food).
transmission(droplet).


transmitted_by(covid_19, airborne).
transmitted_by(covid_19, droplet).
transmitted_by(flu, airborne).
transmitted_by(common_cold, airborne).
transmitted_by(strep_throat, direct_contact).
transmitted_by(norovirus, contaminated_food).

% Symptoms associated with diseases

% For CoVid-19
has_symptom(covid_19, fever).
has_symptom(covid_19, cough).
has_symptom(covid_19, shortness_of_breath).
has_symptom(covid_19, loss_of_taste).
has_symptom(covid_19, loss_of_smell).

% For Common Cold
has_symptom(common_cold, headache).
has_symptom(common_cold, sore_throat).
has_symptom(common_cold, runny_nose).
has_symptom(common_cold, congestion).

% For Flu
has_symptom(flu, fever).
has_symptom(flu, headache).
has_symptom(flu, body_ache).
has_symptom(flu, chills).
has_symptom(flu, sore_throat).
has_symptom(flu, runny_nose).
has_symptom(flu, cough).
has_symptom(flu, shortness_of_breath).

% For Strep Throat
has_symptom(strep_throat, sore_throat).
has_symptom(strep_throat, headache).
has_symptom(strep_throat, nausea).

% For NoroVirus
has_symptom(norovirus, diarrhea).
has_symptom(norovirus, nausea).
has_symptom(norovirus, vomiting).

% Common symptoms
common_symptom(fever).
common_symptom(cough).
common_symptom(sore_throat).

% Rules

% check if specific symptom is associated with a disease
is_symptom_of(Symptom, Disease) :-
    has_symptom(Disease, Symptom).

% check if a disease can be transmitted by a specific method
is_transmitted_by(Disease, Method) :-
    transmitted_by(Disease, Method).

% check if a symptom is common
is_common_symptom(Symptom) :-
    common_symptom(Symptom).

% Find all diseases with a given symptom
diseases_with_symptom(Symptom, Diseases) :-
    findall(Disease, has_symptom(Disease, Symptom), Diseases).

% Find all symptoms associated with a disease
has_symptoms(Disease, Symptoms) :-
    findall(Symptom, has_symptom(Disease, Symptom), Symptoms).

% Rule: Check if a set of symptoms matches a disease
is_diagnosis(Disease, Symptoms) :-
    disease(Disease),
    findall(Symptom, has_symptom(Disease, Symptom), DiseaseSymptoms),
    subset(DiseaseSymptoms, Symptoms).

% Rule: Identify diseases based on symptoms
identify_disease(Symptoms, Disease) :-
    findall(D, (disease(D), is_diagnosis(D, Symptoms)), Diseases),
    member(Disease, Diseases).

% Dynamic predicates for facts and rules
:- dynamic symptom/1, disease/1, has_symptom/2, symptom_weight/3, diagnose/4.

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
symptom(chest_pain).
symptom(wheezing).
symptom(dizziness).
symptom(blurry_vision).

% Diseases
disease(covid_19).
disease(flu).
disease(common_cold).
disease(strep_throat).
disease(norovirus).
disease(asthma).
disease(migraine).

% Symptoms associated with diseases and their weights
has_symptom(covid_19, fever).
has_symptom(covid_19, cough).
has_symptom(covid_19, shortness_of_breath).
has_symptom(covid_19, loss_of_taste).
has_symptom(covid_19, loss_of_smell).

has_symptom(common_cold, headache).
has_symptom(common_cold, sore_throat).
has_symptom(common_cold, runny_nose).
has_symptom(common_cold, congestion).

has_symptom(flu, fever).
has_symptom(flu, headache).
has_symptom(flu, body_ache).
has_symptom(flu, chills).
has_symptom(flu, sore_throat).
has_symptom(flu, runny_nose).
has_symptom(flu, cough).
has_symptom(flu, shortness_of_breath).

has_symptom(strep_throat, sore_throat).
has_symptom(strep_throat, headache).
has_symptom(strep_throat, nausea).

has_symptom(norovirus, diarrhea).
has_symptom(norovirus, nausea).
has_symptom(norovirus, vomiting).

has_symptom(asthma, shortness_of_breath).
has_symptom(asthma, wheezing).
has_symptom(asthma, chest_pain).

has_symptom(migraine, headache).
has_symptom(migraine, dizziness).
has_symptom(migraine, blurry_vision).

symptom_weight(covid_19, loss_of_taste, 10).
symptom_weight(covid_19, fever, 5).
symptom_weight(covid_19, cough, 4).
symptom_weight(covid_19, shortness_of_breath, 8).
symptom_weight(covid_19, loss_of_smell, 6).

symptom_weight(common_cold, headache, 3).
symptom_weight(common_cold, sore_throat, 4).
symptom_weight(common_cold, runny_nose, 5).
symptom_weight(common_cold, congestion, 4).

symptom_weight(flu, fever, 6).
symptom_weight(flu, headache, 5).
symptom_weight(flu, body_ache, 6).
symptom_weight(flu, chills, 5).
symptom_weight(flu, sore_throat, 4).
symptom_weight(flu, runny_nose, 4).
symptom_weight(flu, cough, 5).
symptom_weight(flu, shortness_of_breath, 7).

symptom_weight(strep_throat, sore_throat, 7).
symptom_weight(strep_throat, headache, 4).
symptom_weight(strep_throat, nausea, 5).

symptom_weight(norovirus, diarrhea, 8).
symptom_weight(norovirus, nausea, 7).
symptom_weight(norovirus, vomiting, 8).

symptom_weight(asthma, shortness_of_breath, 8).
symptom_weight(asthma, wheezing, 7).
symptom_weight(asthma, chest_pain, 6).

symptom_weight(migraine, headache, 8).
symptom_weight(migraine, dizziness, 6).
symptom_weight(migraine, blurry_vision, 7).

% Main diagnosis predicate
diagnose(PatientSymptoms, _Diagnoses, Justifications) :-
    findall(Disease, disease(Disease), Diseases),
    diagnose_diseases(PatientSymptoms, Diseases, DiagnosesWithCertainty),
    sort_diagnoses(DiagnosesWithCertainty, SortedDiagnosesWithCertainty),
    extract_justifications(SortedDiagnosesWithCertainty, Justifications).

% Diagnose diseases and collect all with their certainties and justifications
diagnose_diseases(_, [], []).
diagnose_diseases(PatientSymptoms, [Disease|Rest], [Disease-Certainty-Justification|Diagnoses]) :-
    calculate_certainty(Disease, PatientSymptoms, RawCertainty),
    normalize_certainty(RawCertainty, Certainty),
    (Certainty > 0 ->
        justification(Disease, PatientSymptoms, Certainty, Justification),
        Diagnoses = [Disease-Certainty-Justification | RestDiagnoses]
    ;
        Diagnoses = RestDiagnoses
    ),
    diagnose_diseases(PatientSymptoms, Rest, RestDiagnoses).

% Sort diagnoses by certainty score in descending order
sort_diagnoses(Diagnoses, SortedDiagnoses) :-
    predsort(compare_certainty, Diagnoses, SortedDiagnoses).

% Comparator for sorting diagnoses
compare_certainty(Delta, _-Certainty1-_, _-Certainty2-_) :-
    compare(Delta, Certainty2, Certainty1).

% Extract justifications from sorted diagnoses
extract_justifications([], []).
extract_justifications([_-_-Justification | Rest], [Justification | Justifications]) :-
    extract_justifications(Rest, Justifications).

% Calculate certainty of a diagnosis
calculate_certainty(Disease, PatientSymptoms, Certainty) :-
    findall(WeightedSymptomScore, (
        member(Symptom, PatientSymptoms),
        has_symptom(Disease, Symptom),
        symptom_weight(Disease, Symptom, SymptomWeight),
        WeightedSymptomScore is SymptomWeight
    ), WeightedScores),
    sum_list(WeightedScores, TotalWeight),
    length(PatientSymptoms, NumSymptoms),
    (NumSymptoms > 0 ->
        RawCertainty is TotalWeight / NumSymptoms,
        normalize_certainty(RawCertainty, Certainty)
    ;
        Certainty is 0
    ).

% Normalize certainty score to a maximum of 10
normalize_certainty(RawCertainty, Certainty) :-
    Certainty is min(10, RawCertainty). % Ensure Certainty is at most 10

% Justification for diagnosis
justification(Disease, PatientSymptoms, Certainty, Justification) :-
    findall(Symptom, (member(Symptom, PatientSymptoms), has_symptom(Disease, Symptom)), MatchingSymptoms),
    atomic_list_concat(MatchingSymptoms, ', ', SymptomList),
    format(atom(Justification), '~w|~w|~2f', [Disease, SymptomList, Certainty]).

% Helper function to sum a list
sum_list(List, Sum) :-
    sum_list(List, 0, Sum).

sum_list([], Sum, Sum).
sum_list([H|T], Acc, Sum) :-
    Acc1 is Acc + H,
    sum_list(T, Acc1, Sum).

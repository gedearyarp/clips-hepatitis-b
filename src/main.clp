(deffunction ProposeQuestion (?question $?allowed-values)
	(printout t ?question)
	(bind ?answer (read))
	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))
	(while (not (member$ ?answer ?allowed-values)) do
		(printout t ?question)
		(bind ?answer (read))
		(if (lexemep ?answer)
			then (bind ?answer (lowcase ?answer))))
	?answer
)

(deffunction positiveOrNegative (?question)
	(bind ?response (ProposeQuestion ?question positive negative p n pos neg))
	(if (or (eq ?response positive) (eq ?response p) (eq ?response pos))
		then positive
		else negative)
)

(deftemplate Symptoms
	(slot symptom-id)
)

(deftemplate Diagnosis
	(slot diagnosis-name)
	(multislot input-symptoms)
)

; ==================
; (0 neg 1 pos)
; HBsAg: 1 
; AntiHDV 2
; AntiHBc 3
; AntiHBs 4
; IgmAntiHbc 5
; ==================

(deffacts SymptomDiagnosis
    (Diagnosis(diagnosis-name "Acute Infection")(input-symptoms 11 20 31 40 51))
    (Diagnosis(diagnosis-name "Chronic Infection")(input-symptoms 11 20 31 40 50))
    (Diagnosis(diagnosis-name "Hepatitis B+D")(input-symptoms 11 21))
    (Diagnosis(diagnosis-name "Uncertain configuration ")(input-symptoms 11 20 30))
    (Diagnosis(diagnosis-name "Uncertain configuration")(input-symptoms 11 20 31 41))
    (Diagnosis(diagnosis-name "Cured")(input-symptoms 10 31 41))
    (Diagnosis(diagnosis-name "Vaccinated")(input-symptoms 10 30 41))
    (Diagnosis(diagnosis-name "Unclear (possible resolved)")(input-symptoms 10 31 40))
    (Diagnosis(diagnosis-name "Healthy not vaccinated or suspicious")(input-symptoms 10 30 40))
)

(defrule GetHBsAgStatus
    (not (HBsAg ?))
    =>
    (assert(HBsAg(positiveOrNegative "HBsAg? ")))
)

(defrule GetAntiHDVStatusLeft
    (HBsAg positive)
    (not(AntiHDV ?))
    =>
    (assert(Symptoms(symptom-id 11)))
    (assert(AntiHDV(positiveOrNegative "Anti-HDV? ")))
)

(defrule GetAntiHBcStatusLeft
    (HBsAg positive)
    (AntiHDV negative)
    (not(AntiHBc ?))
    =>
    (assert(Symptoms(symptom-id 20)))
    (assert(AntiHBc(positiveOrNegative "Anti-HBc? ")))
)

(defrule GetFinalHepatitisBD
    (HBsAg positive)
    (AntiHDV positive)
    =>
    (assert(Symptoms(symptom-id 21)))
)

(defrule GetAntiHBsStatusLeft
    (HBsAg positive)
    (AntiHDV negative)
    (AntiHBc positive)
    (not(AntiHBs ?))
    =>
    (assert(Symptoms(symptom-id 31)))
    (assert(AntiHBs(positiveOrNegative "Anti-HBs? ")))
)

(defrule GetFinalUncertainRight
    (HBsAg positive)
    (AntiHDV negative)
    (AntiHBc negative)
    =>
    (assert(Symptoms(symptom-id 30)))
)

(defrule GetFinalUncertainLeft
    (HBsAg positive)
    (AntiHDV negative)
    (AntiHBc positive)
    (AntiHBs positive)
    =>
    (assert(Symptoms(symptom-id 41)))
)

(defrule GetIgmAntiHbcStatusLeft
    (HBsAg positive)
    (AntiHDV negative)
    (AntiHBc positive)
    (AntiHBs negative)
    (not(IgmAntiHbc ?))
    =>
    (assert(Symptoms(symptom-id 40)))
    (assert(IgmAntiHbc(positiveOrNegative "Igm Anti-Hbc? ")))
)

(defrule GetFinalAcuteInfection
    (HBsAg positive)
    (AntiHDV negative)
    (AntiHBc positive)
    (AntiHBs negative)
    (IgmAntiHbc positive)
    =>
    (assert(Symptoms(symptom-id 51)))
)

(defrule GetFinalChronicInfection
    (HBsAg positive)
    (AntiHDV negative)
    (AntiHBc positive)
    (AntiHBs negative)
    (IgmAntiHbc negative)
    =>
    (assert(Symptoms(symptom-id 50)))
)

(defrule GetAntiHBsStatusRight
    (HBsAg negative)
    (not(AntiHBs ?))
    =>
    (assert(Symptoms(symptom-id 10)))
    (assert(AntiHBs(positiveOrNegative "Anti-HBs? ")))
)

(defrule GetAntiHBcStatusAntiHBsRight
    (HBsAg negative)
    (AntiHBs positive)
    (not(AntiHBc ?))
    =>
    (assert(Symptoms(symptom-id 41)))
    (assert(AntiHBc(positiveOrNegative "Anti-HBc? ")))
)

(defrule GetAntiHBcStatusRight
    (HBsAg negative)
    (AntiHBs negative)
    (not(AntiHBc ?))
    =>
    (assert(Symptoms(symptom-id 40)))
    (assert(AntiHBc(positiveOrNegative "Anti-HBc? ")))
)

(defrule GetFinalRight
    (HBsAg negative)
    (AntiHBc positive)
    =>
    (assert(Symptoms(symptom-id 31)))
)

(defrule GetFinalRightNegative
    (HBsAg negative)
    (AntiHBc negative)
    =>
    (assert(Symptoms(symptom-id 30)))
)

(defrule All-symptoms
	(Diagnosis(diagnosis-name ?name))
	(forall(Diagnosis(diagnosis-name ?name)(input-symptoms $? ?symptom $?))
		(Symptoms (symptom-id ?symptom))) 
	=>
		(printout t "Hasil Prediksi = " ?name crlf)
)
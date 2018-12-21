;------- Structures -------

(defstruct medicament 
   nom
   dose
   type
)

(defstruct symptome
   nom 
   intensité
)

(defstruct pathologie 
   nom 
)

(defstruct regle
    nom
    poids  ; Plus le poids est élevé, plus la règle a de l'importance
    condition
    action
)

;------- Fonctions -------

(defun membrep (e l)
  (cond ((atom l) nil)
        ((equal e (car l)) t)
        (t (membrep e (cdr l)))
  )
)

(defun trouveReglesActivables (listeRegles listeReglesActivables)
  (cond ((atom listeRegles) listeReglesActivables)
        ((eval (regle-condition (car listeRegles))) (trouveReglesActivables (cdr listeRegles) (cons (car listeRegles) listeReglesActivables)))
        (t (trouveReglesActivables (cdr listeRegles) listeReglesActivables))
  )
)

; Nécessaire pour trier les règles dans l'ordre décroissant (sur le poids)
(defun compareRegleGT (r1 r2)
    (cond ((< (regle-poids r1) (regle-poids r2)) nil)
          (t t)
    )
)

(defun retireElementListe (element liste)
    (cond ((atom liste) ())
        ((equal (regle-nom element) (regle-nom (car liste))) (retireElementListe element (cdr liste)))
        (T (cons (car liste) (retireElementListe element (cdr liste))))))
  

;------- Medicaments -------

(setq ibuprofene (make-medicament
  :nom "Ibuprofène"
  :dose 1
  :type "anti-inflamatoire")
)

(setq paracetamol (make-medicament
  :nom "Paracetamol"
  :dose 1
  :type "antalgique")
)

(setq ventoline (make-medicament
  :nom "Ventoline"
  :dose 1
  :type "bronchodilatateur")
)
    
;------- Symptomes -------

(setq rougeur (make-symptome
                  :nom "rougeur"
              )
)

(setq gonflement (make-symptome
                  :nom "gonflement"
              )
)

(setq sensation_chaleur (make-symptome
                  :nom "sensation de chaleur"
              )
)

(setq difficultes_respiratoire (make-symptome
                  :nom "difficultes respiratoires"
              )
)

(setq douleur (make-symptome
                  :nom "douleur"
                  :intensité 2
              )
)
    
;------- Pathologies -------
    
(setq inflammation (make-pathologie
  :nom "inflammation"
  )
)
    
(setq asthme (make-pathologie
  :nom "asthme"
  )
)
    
;------- Regles Medicament -------

(setq regleAdministrationVentoline (make-regle
  :nom "regle administration Ventoline"
  :poids 1
  :condition '(membrep 'asthme liste_pathologies)
  :action '(setq liste_medicaments (cons 'ventoline liste_medicaments)))
)

(setq regleAdministrationIbuprofene (make-regle
  :nom "regle administration Ibuprofène"
  :poids 1
  :condition '(or (membrep 'inflammation liste_pathologies) (and (membrep 'douleur liste_symptomes) (< 3 (symptome-intensité douleur))))
  :action '(setq liste_medicaments (cons 'ibuprofene liste_medicaments)))
)

(setq regleAdministrationParacetamol (make-regle
  :nom "regle administration Paracetamol"
  :poids 0
  :condition '(and (membrep 'douleur liste_symptomes) (<= (symptome-intensité douleur) 3))
  :action '(setq liste_medicaments (cons 'paracetamol liste_medicaments))))

;------- Regles Symptomes -------

(setq regleDiagnosticAsthme (make-regle
  :nom "regle diagnostic asthme"
  :poids 2
  :condition '(membrep 'difficultes_respiratoire liste_symptomes)
  :action '(setq liste_pathologies (cons 'asthme liste_pathologies)))
)

(setq regleDiagnosticInflammation (make-regle
  :nom "regle diagnostic inflammation"
  :poids 2
  :condition '(and (membrep 'rougeur liste_symptomes) (membrep 'gonflement liste_symptomes))
  :action '(setq liste_pathologies (cons 'inflammation liste_pathologies)))
)

;------- Listes (état du monde) -------

; Symptomes disponibles : douleur, rougeur, gonflement, sensation_chaleur, difficultes_respiratoire
; Base de faits :
(setq liste_symptomes (list 'gonflement 'rougeur 'difficultes_respiratoire 'douleur))
(setq liste_pathologies ())
(setq liste_medicaments ())
(setq liste_regles (list regleDiagnosticInflammation regleAdministrationVentoline regleDiagnosticAsthme regleAdministrationIbuprofene regleAdministrationParacetamol))
(setq liste_regles_en_conflit ())

    
;------- Moteur d'inférences -------

(loop
    ; Détermination de l'ensemble des conflits
    (setq liste_regles_en_conflit (trouveReglesActivables liste_regles ()))
      
    ; On trie la liste des conflits en fonction du poid des règles (le poid le plus grand en premier)
    (setq liste_regles_en_conflit (sort liste_regles_en_conflit #'compareRegleGT))
    ; Vérification de la condition d'ARRET
    (when (atom liste_regles_en_conflit) (return nil))
  
    ; On effectue l'action associé à la règle en tète de liste
    (setq regle_courante (car liste_regles_en_conflit))
    (eval (regle-action regle_courante))
    (print "regle exécutée :")
    (print (regle-nom regle_courante))
    ; Une fois la règle effectuée on l'enlève de la liste des règles
    (setq liste_regles (retireElementListe regle_courante liste_regles)))

(print "LOOP DONE")
(print "pathologies detectées :")
(print liste_pathologies)
(print "prescription :")
(print liste_medicaments)
(defun membrep (l1 l2)
  (cond ((atom l2) nil)
        ((equal l1 (car l2)) t)
        (t (membrep l1 (cdr l2)))
  )
)

(defun trouveReglesActivables (listeRegles listeReglesActivables)
  (cond ((atom listeRegles) listeReglesActivables)
<<<<<<< HEAD
        ((eval (regle-condition (car listeRegles))) (trouveReglesActivables (cdr listeRegles) (cons (car listeRegles) listeReglesActivables)))
        (t (trouveReglesActivables (cdr listeRegles) listeReglesActivables))
=======
        ((regle-condition (car listeRegles)) (append (car listeRegles) listeReglesActivables) (trouveReglesActivables (cdr listeRegles) listeReglesActivables))
        (t (membrep l1 (cdr l2)))
>>>>>>> master
  )
)

(defun compareRegleGT (r1 r2)
<<<<<<< HEAD
    (cond ((> (regle-poids r1) (regle-poids r2)) nil)
          (t t)
    )
)

(defun retireElementListe (element liste)
    (cond ((atom liste) ())
        ((equal (regle-nom element) (regle-nom (car liste))) (retireElementListe element (cdr liste)))
        (T (cons (car liste) (retireElementListe element (cdr liste))))))
  
=======
    (cond ((> (regle-poids r1) (regle-poids r2)) r1)
          (t r2)
    )
)

    
>>>>>>> master

;------------------------------------------
    
(defstruct medicament 
   nom
   dose
   type
)

(setq ibuprofene (make-medicament
  :nom "Ibuprofene"
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
    
;------------------------------------------

(defstruct symptome
   nom 
)

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
    
;------------------------------------------
    
(defstruct pathologie 
   nom 
)
    
(setq inflammation (make-pathologie
  :nom "inflammation"
  )
)
    
(setq asthme (make-pathologie
  :nom "asthme"
  )
)
    
;------------------------------------------
    
(defstruct regle
    nom
    poids
    condition
    action
)

<<<<<<< HEAD
(setq regleAdministrationVentoline (make-regle
  :nom "regle administration Ventoline"
  :poids 1
  :condition '(membrep asthme liste_pathologies)
  :action '(print 'ventoline) )
)

(setq regleDiagnosticAsthme (make-regle
  :nom "regle diagnostic asthme"
  :poids 2
  :condition '(membrep difficultes_respiratoire liste_symptomes)
  :action (cons 'asthme '(liste_pathologies)) )
)

(setq regleDiagnosticInflammation (make-regle
  :nom "regle diagnostic inflammation"
  :poids 2
  :condition '(membrep rougeur liste_symptomes)
  :action (cons 'inflammation '(liste_pathologies)) )
)
;------------------------------------------

; Symptomes disponibles : rougeur gonflement sensationChaleur difficultesRespiratoire
; Base de faits :
(setq liste_symptomes (list 'difficultesRespiratoire))
(setq liste_pathologies ())
(setq liste_medicaments ())
(setq liste_regles (list regleDiagnosticInflammation regleAdministrationVentoline regleDiagnosticAsthme))
(setq liste_regles_en_conflit ())

    
; Moteur d'inférence

;(setq ARRET nil)

(loop
   ; Vérification de la condition d'arret
    ;(when ARRET (return nil))
   ; Détermination de l'ensemble des conflits
    (setq liste_regles_en_conflit (trouveReglesActivables liste_regles ()))
     (print liste_regles_en_conflit)
     
   ; On trie la liste des conflits en fonction du poid des règles (le poid le plus grand en premier)
   (setq liste_regles_en_conflit (sort liste_regles_en_conflit #'compareRegleGT))
    ; BREAK MOCHE
     (when (atom liste_regles_en_conflit) (return nil))
 
   ; On effectue l'action associé à la règle en tète de liste
   (setq regle (car liste_regles_en_conflit))
   (print (regle-action regle))
   (retireElementListe regle liste_regles)
=======
(setq regle1 (make-regle
  :nom "Regle 1"
  :poids 1
  :condition (membrep 'asthme '(liste_pathologies))
  :action ventoline )
)

(setq regle2 (make-regle
  :nom "Regle 2"
  :poids 2
  :condition 'difficultes_respiratoire
  :action (cons 'asthme '(liste_pathologies)) )
)

(setq regle3 (make-regle
  :nom "Regle 3"
  :poids 3
  :condition 'rougeur
  :action (cons 'inflammation '(liste_pathologies)) )
>>>>>>> master
)
;------------------------------------------

; Symptomes disponibles : rougeur gonflement sensationChaleur difficultesRespiratoire
; Base de faits :
;(setq liste_symptomes ('difficultesRespiratoire))
(setq liste_pathologies ())
(setq liste_medicaments ())
(setq liste_regles (list regle3 regle2 regle1))
(setq liste_regles_en_conflit ())

    
; Moteur d'inférence

(setq ARRET nil)

(loop
   ; Vérification de la condition d'arret
    (when '(not ARRET) (return nil))
   ; Détermination de l'ensemble des conflits
     
   ; On trie la liste des conflits en fonction du poid des règles
     ;TODO
   ; On effectue l'action associé à la règle en tète de liste
     ;TODO
   
)


;(print (compareRegleGT regle1 regle2))
(print (sort liste_regles
      (lambda (x y) (< (regle-poids x) (regle-poids y)))))

<<<<<<< HEAD
(print "DONE")
(print (eval (regle-condition regleDiagnosticAsthme)))
=======
;(print (sort liste_regles #'compareRegleGT))
;(print liste_regles)
;(print (eval (regle-nom (compareregleGT )))
>>>>>>> master

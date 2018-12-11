(defstruct drug 
   name 
   dose
)

(setq paracetamol (make-drug
  :name "Paracetamol"
  :dose 1 )
)

(setq ibuprofene (make-drug
  :name "Ibuprofene"
  :dose 1 )
)

(defstruct rule
    name
    weight
    condition
    action
)

(setq migraine t)

(setq rule1 (make-rule
  :name "Rule 1"
  :weight 1
  :condition 'migraine
  :action paracetamol )
)

(print (eval (rule-condition rule1)))
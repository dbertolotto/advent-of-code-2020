(setq Inp (in "12.txt" (till NIL)))
(setq List (mapcar '((X) (cons (car X) (format (pack (cdr X))))) (filter 'and (split Inp "\n"))))

(setq Demo '(("F" . 10) ("N" . 3) ("F" . 7) ("R" . 90) ("F" . 11)))

# 12a
(setq Ship '("E" 0 0))

(de turn (A V)
  (let ((D X Y) Ship
        Rose '("N" "E" "S" "W" .)
        Val (/ V 90)
        Dir (if (= A "L") '- '+)
        Idx (index D Rose)
        N (Dir (+ Idx 4) Val)
        NewD (car (nth Rose N)))
    (setq Ship (list NewD X Y))))

(de move (V)
  (let ((D X Y) Ship)
    (case D
      ("N" (setq Ship (list D X (+ Y V))))
      ("S" (setq Ship (list D X (- Y V))))
      ("E" (setq Ship (list D (+ X V) Y)))
      ("W" (setq Ship (list D (- X V) Y))))))
    
(de act (Action)
  (let ((A . V) Action
        (D X Y) Ship)
    (case A
      ("N" (setq Ship (list D X (+ Y V))))
      ("S" (setq Ship (list D X (- Y V))))
      ("E" (setq Ship (list D (+ X V) Y)))
      ("W" (setq Ship (list D (- X V) Y)))
      ("L" (turn A V))
      ("R" (turn A V))
      ("F" (move V)))))

(de manhattan ()
  (let ((D X Y) Ship)
    (+ (abs X) (abs Y))))

(setq Ship '("E" 0 0))
(mapcar 'act Demo)
(test 25 (manhattan))

(setq Ship '("E" 0 0))
(mapcar 'act List)
(println "distance: " (manhattan))

# 12b
(setq Ship '("E" 0 0))
(setq Way '(10 1))

(de rotate (A V)
  (let ((XW YW) Way)
    (case V
      (90  (setq Way (if (= A "R") (list YW (- XW)) (list (- YW) XW))))
      (180 (setq Way (list (- XW) (- YW))))
      (270  (setq Way (if (= A "L") (list YW (- XW)) (list (- YW) XW)))))))

(de act2 (Action)
  (let ((A . V) Action
        (D X Y) Ship
        (XW YW) Way)
    (case A
      ("N" (setq Way (list XW (+ YW V))))
      ("S" (setq Way (list XW (- YW V))))
      ("E" (setq Way (list (+ XW V) YW)))
      ("W" (setq Way (list (- XW V) YW)))
      ("L" (rotate A V))
      ("R" (rotate A V))
      ("F" (setq Ship (list D (+ X (* V XW)) (+ Y (* V YW))))))))

(setq Ship '("E" 0 0))
(setq Way '(10 1))
(mapcar 'act2 Demo)
(test 286 (manhattan))

(setq Ship '("E" 0 0))
(setq Way '(10 1))
(mapcar 'act2 List)
(println "distance: " (manhattan))

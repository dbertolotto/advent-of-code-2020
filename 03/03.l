(setq Inp (in "03.txt" (till NIL T)))
(setq List (filter 'and (mapcar '((X) (apply 'circ X)) (split (chop Inp) "\n"))))

# 3a
(de count-tree (Row+ Pos+)
  (let (Row 1 Pos 1 Trees 0)
    (while (car (nth List Row))
      (setq Line @)
      (when (= "#" (car (nth Line Pos))) (setq Trees (+ 1 Trees)))
      (setq Row (+ Row+ Row) Pos (+ Pos+ Pos)))
    (prinl "trees(" Pos+ "," Row+ "): " Trees)
    Trees))
(count-tree 1 3)
  
# 3b
(setq Results
  (make 
    (for P '((1 . 1) (1 . 3) (1 . 5) (1 . 7) (2 . 1))
      (link (count-tree (car P) (cdr P))))))
(prinl "* all: " (apply * Results))
  

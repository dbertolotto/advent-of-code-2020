(setq Inp (in "05.txt" (till NIL T)))
(setq List (filter 'and (split (chop Inp) "\n")))

(de parse (Pass)
  (let (Row (head 7 Pass)
        Col (tail 3 Pass)
        RG (range 0 127)
        CG (range 0 7))
    (for R Row
      (setq RG ((if (= R "F") 'head 'tail) (/ (length RG) 2) RG)))
    (for C Col
      (setq CG ((if (= C "L") 'head 'tail) (/ (length CG) 2) CG)))
    (list (car RG) (car CG) (+ (car CG) (* 8 (car RG))))))

(test '(70 7 567) (parse (chop "BFFFBBFRRR")))
(test '(14 7 119) (parse (chop "FFFBBBFRRR")))
(test '(102 4 820) (parse (chop "BBFFBBFRLL")))

(setq List2 (mapcar 'parse List))
      
# 5a
(prinl "max id " (apply 'max (mapcar 'last List2)))

# 5b
(de check (S1 S2)
  (ifn (= 1 (- S2 S1))
    (prinl "seat " (dec S2))))
    
(let (Ids (sort (mapcar 'last List2))
      L1 (head (dec (length Ids)) Ids)
      L2 (tail (dec (length Ids)) Ids))
  (mapcar 'check L1 L2))
      

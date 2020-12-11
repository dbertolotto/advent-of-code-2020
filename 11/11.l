(setq Inp (in "11.txt" (till NIL)))
(setq List (filter 'and (split Inp "\n")))

# 11a
(setq Demo (filter 'and (split (in "demo.txt" (till)) "\n")))

(de seat-round (Lst)
  (make
    (for (I . R) Lst
      (link (make
        (for (J . C) R
          (let Cnt 0
            # same line
            (when (= (car (nth R (- J 1))) "#") (setq Cnt (inc Cnt)))
            (when (= (car (nth R (+ J 1))) "#") (setq Cnt (inc Cnt)))
            # above
            (when (= (car (nth (car (nth Lst (- I 1))) (- J 1))) "#") (setq Cnt (inc Cnt)))
            (when (= (car (nth (car (nth Lst (- I 1))) (+ J 1))) "#") (setq Cnt (inc Cnt)))
            (when (= (car (nth (car (nth Lst (- I 1))) J)) "#") (setq Cnt (inc Cnt)))
            # below
            (when (= (car (nth (car (nth Lst (+ I 1))) (- J 1))) "#") (setq Cnt (inc Cnt)))
            (when (= (car (nth (car (nth Lst (+ I 1))) (+ J 1))) "#") (setq Cnt (inc Cnt)))
            (when (= (car (nth (car (nth Lst (+ I 1))) J)) "#") (setq Cnt (inc Cnt)))
            # check
            #(println I J C)
            (cond
              ((and (= C "L") (= Cnt 0)) (link "#"))
              ((and (= C "#") (> Cnt 3)) (link "L"))
              (T (link C))))))))))

(de print-seats (Lst)
  (for R Lst
    (for C R
      (prin C))
    (prinl)))

(de compare-seats (OldLst NewLst)
  (= (fish 'atom OldLst) (fish 'atom NewLst)))

(de count-seats (Lst)
  (length (filter '((C) (= C "#")) (fish 'atom Lst))))

(de loop-seats (Lst)
  (let (OldLst Lst
        NewLst (seat-round OldLst))
    (until (compare-seats OldLst NewLst)
      (setq OldLst NewLst)
      (setq NewLst (seat-round OldLst)))
    NewLst))

(test 37 (count-seats (loop-seats Demo)))

(prinl "occupied: " (count-seats (loop-seats List)))

# 11b
(de diag+ (Lst I J)
  (let (NI (length Lst)
        NJ (length (car Lst)))
    (by 'cadr 'sort 
      (uniq (make
        (let (SI I SJ J)
          (while (and (<= SJ NJ) (> SI 0))
            (link (list SI SJ (car (nth (car (nth Lst SI)) SJ))))
            (setq SI (dec SI) SJ (inc SJ))))
        (let (SI I SJ J)
          (while (and (> SJ 0) (<= SI NI))
            (link (list SI SJ (car (nth (car (nth Lst SI)) SJ))))
            (setq SI (inc SI) SJ (dec SJ))))
        )))))

(de diag- (Lst I J)
  (let (NI (length Lst)
        NJ (length (car Lst)))
    (by 'cadr 'sort 
      (uniq (make
        (let (SI I SJ J)
          (while (and (<= SJ NJ) (<= SI NI))
            (link (list SI SJ (car (nth (car (nth Lst SI)) SJ))))
            (setq SI (inc SI) SJ (inc SJ))))
        (let (SI I SJ J)
          (while (and (> SJ 0) (> SI 0))
            (link (list SI SJ (car (nth (car (nth Lst SI)) SJ))))
            (setq SI (dec SI) SJ (dec SJ))))
        )))))

(de seat-round-2 (Lst)
  (make
    (for (I . R) Lst
      (link (make
        (for (J . C) R
          (let (Cnt 0
                Vert  (mapcar '((X) (car (nth X J))) Lst)
                Diag+ (filter '((X) (not (= (caddr X) "."))) (diag+ Lst I J))
                Diag- (filter '((X) (not (= (caddr X) "."))) (diag- Lst I J)))
            # same line
            (when (= "#" (last (filter '((X) (not (= X "."))) (head (- J 1) R)))) (setq Cnt (inc Cnt)))
            (when (= "#" (car  (filter '((X) (not (= X "."))) (tail (- J)   R)))) (setq Cnt (inc Cnt)))
            # diag
            (when (= "#" (last (mapcar 'caddr (filter '((X) (< (cadr X) J)) Diag+)))) (setq Cnt (inc Cnt)))
            (when (= "#" (car  (mapcar 'caddr (filter '((X) (> (cadr X) J)) Diag+)))) (setq Cnt (inc Cnt)))
            (when (= "#" (last (mapcar 'caddr (filter '((X) (< (cadr X) J)) Diag-)))) (setq Cnt (inc Cnt)))
            (when (= "#" (car  (mapcar 'caddr (filter '((X) (> (cadr X) J)) Diag-)))) (setq Cnt (inc Cnt)))
            # vert
            (when (= "#" (last (filter '((X) (not (= X "."))) (head (- I 1) Vert)))) (setq Cnt (inc Cnt)))
            (when (= "#" (car  (filter '((X) (not (= X "."))) (tail (- I)   Vert)))) (setq Cnt (inc Cnt)))
            # check
            #(println I J C)
            (cond
              ((and (= C "L") (= Cnt 0)) (link "#"))
              ((and (= C "#") (> Cnt 4)) (link "L"))
              (T (link C))))))))))
      
(de loop-seats-2 (Lst)
  (let (OldLst Lst
        NewLst (seat-round-2 OldLst))
    (until (compare-seats OldLst NewLst)
      (setq OldLst NewLst)
      (setq NewLst (seat-round-2 OldLst)))
    NewLst))
 
(test 26 (count-seats (loop-seats-2 Demo)))

(prinl "occupied: " (count-seats (loop-seats-2 List)))

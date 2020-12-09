(setq Inp (in "09.txt" (till NIL T)))
(setq List (mapcar 'format (filter 'and (split (chop Inp) "\n"))))

# 9a
(setq Preamble (head 25 List))
(setq Numbers (tail -25 List))

(setq TestList (35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576))
(setq TestPreamble (head 5 TestList))
(setq TestNumbers (tail -5 TestList))

(de sums (Lst)
  (make
    (for (I . L) Lst Lst (pop 'Lst)
      (mapcar '((X) (link (+ L X))) Lst))))

(de check (Lst Pre)
  (for (I . L) Lst Lst (pop 'Lst)
    #(println L Pre (if (member L (sums Pre)) T NIL))
    (NIL (member L (sums Pre)) (prinl "number: " L))
    (setq Pre (remove 1 Pre))
    (setq Pre (append Pre (list L)))
    ))

(out "/dev/null" (test 127 (check TestNumbers TestPreamble)))

(setq Target (check Numbers Preamble))

# 9b
(de scanit (Target Lst)
  (for I (range 2 (length Lst))
    (setq Res (maplist '((L) (list (apply '+ (head I L)) (head I L))) Lst))
    (T (assoc Target Res) (let Set (cadr @) (prinl "value: " (+ (apply 'min Set) (apply 'max Set)))))
  ))

(out "/dev/null" (test 62 (scanit 127 TestList)))

(scanit Target List)

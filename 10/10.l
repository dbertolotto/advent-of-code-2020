(setq Inp (in "10.txt" (till NIL T)))
(setq List (mapcar 'format (filter 'and (split (chop Inp) "\n"))))

# 10a
(setq TList1 (16 10 15 5 1 11 7 19 6 12 4))
(setq TList2 (28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3))

(de compute (List)
  (let (SList (sort (copy List))
        ExtList (insert 0 (insert (inc (length SList)) SList (+ 3 (last SList))) 0)
        Diffs (mapcar - (tail -1 ExtList) (head -1 ExtList))
        Jolts (by format group Diffs))
    (* (length (assoc 1 Jolts)) (length (assoc 3 Jolts)))))

(test 35 (compute TList1))
(test 220 (compute TList2))

(prinl "answer: " (compute List))

# 10b
(de combine (List)
  (let (SList (sort (copy List))
        Sol '((0 . 1)))
    (for I SList
      (push 'Sol (cons I 0))
      (if (assoc (- I 1) Sol)
        (push 'Sol (cons I (+ (cdr (assoc I Sol)) (cdr (assoc (- I 1) Sol))))))
      (if (assoc (- I 2) Sol)
        (push 'Sol (cons I (+ (cdr (assoc I Sol)) (cdr (assoc (- I 2) Sol))))))
      (if (assoc (- I 3) Sol)
        (push 'Sol (cons I (+ (cdr (assoc I Sol)) (cdr (assoc (- I 3) Sol)))))))
    (assoc (last SList) Sol)))

(test 8 (cdr (combine TList1)))
(test 19208 (cdr (combine TList2)))

(prinl "answer: " (cdr (combine List)))

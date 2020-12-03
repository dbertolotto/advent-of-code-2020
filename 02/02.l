(setq Inp (in "02.txt" (till NIL T)))
(setq List (filter 'and (mapcar '((X) (pack X)) (split (chop Inp) "\n"))))

# 2a
(de parse (S)
  (let ((LowToHigh Letter Password) (split (chop S) " ")
        (Low High) (split LowToHigh "-"))
    (list (format (pack Low)) (format (pack High)) (car Letter) Password)))

(de check (L)
  (let ((Low High Letter Password) L
        Occurs (length (filter '((X) (= Letter X)) Password)))
    (and (>= Occurs Low) (<= Occurs High))))

(prinl "valid: " (length (filter 'and (mapcar '((S) (check (parse S))) List))))

# 2b
(de check2 (L)
  (let ((P1 P2 Letter Password) L)
    (xor (= Letter (car (nth Password P1)))
         (= Letter (car (nth Password P2))))))

(prinl "valid: " (length (filter 'and (mapcar '((S) (check2 (parse S))) List))))

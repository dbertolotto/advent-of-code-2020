(setq Inp (in "01.txt" (till NIL T)))
(setq List (filter 'and (mapcar '((X) (format (pack X))) (split (chop Inp) "\n"))))

# 1a
(for I List
  (T (for J List
       (T (= 2020 (+ I J)) (prinl I " * " J " = " (* I J))))))

# 1b
(for I List
  (T (for J List
    (T (for K List
         (T (= 2020 (+ I J K)) (prinl I " * " J " * " K " = " (* I J K)))) T))))


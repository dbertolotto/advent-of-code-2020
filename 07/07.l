(setq Inp (in "07.txt" (till NIL T)))
(setq List (filter 'and (split (chop Inp) "\n")))

# 7a
(de parse (Line)
  (filter '((X) (not (member X '("bag" "bags" "bag," "bags," "bag." "bags." "contain"))))
          (mapcar pack (split Line " "))))

(de to-data (Line)
  (make
    (link (pack (car Line)" " (cadr Line))) 
    (let Cont (tail -2 Line)
      (while Cont
        (let Piece (cut 3 'Cont)
          (if (format (car Piece))
            (link (cons (pack (cadr Piece) " " (caddr Piece)) @))))))))

(test '("dark beige" ("posh lime" . 2) ("striped black" . 3))
      (to-data '("dark" "beige" "2" "posh" "lime" "3" "striped" "black")))
(test '("shiny indigo")
      (to-data '("shiny" "indigo" "no" "other")))

(setq List2 (mapcar 'to-data (mapcar 'parse List)))

(de content (Entry)
  (make
    (link (car Entry))
    (expand (cdr Entry) 1)))

(de expand (Bags N)
  (for B Bags
    (let ((Name . Count) B
          NewN (* Count N)
          SubBags (cdr (assoc Name List2)))
    (link (cons Name NewN))
    (expand SubBags NewN))))

(setq TestList '(("shiny gold" ("dull magenta" . 2)) ("dull magenta" ("drab purple" . 3)) ("drab purple")))
(test '("shiny gold" ("dull magenta" . 2) ("drab purple" . 6))
      (let List2 TestList (content (car TestList))))

(setq List3 (mapcar 'content List2))

(prinl "N: " (length (filter '((X) (assoc "shiny gold" X)) List3)))

# 7b
(prinl "N: " (apply '+ (mapcar 'cdr (cdr (content (assoc "shiny gold" List2))))))

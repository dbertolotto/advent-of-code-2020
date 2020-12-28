(setq Inp (in "21.txt" (till NIL)))
(setq Blocks (filter 'and (split Inp "\n")))

(setq DemoInp (in "demo.txt" (till NIL)))
(setq DemoBlocks (filter 'and (split DemoInp "\n")))

# 21a
(de parse (Food)
  (let (Line (str (pack Food))
        Ingredients (filter atom Line)
        Allergenes (cdr (last Line)))
    (for A Allergenes
      (for I Ingredients
        (push '*Ingr (cons A I))))))

(de parse-all (Blocks)
  (let *Ingr '()
    (uniq (mapcar parse Blocks))
    *Ingr))

(setq Ingr (parse-all Blocks))
(setq DemoIngr (parse-all DemoBlocks))

(de set-of (@Allergen Ingr)
  (macro
    (uniq
      (mapcar cdr
              (filter '((X) (= (car X) '@Allergen))
                      Ingr)))))

(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

(de allergenes (Ingr)
  (uniq (mapcar car Ingr)))

(de ingredients (Ingr)
  (uniq (mapcar cdr Ingr)))

(de sets (Ingr)
  (mapcar '((A) (set-of A Ingr)) (allergenes Ingr)))

(setq *Safe '())

(de find-safe (Ingr)
  (let (Safe (car (reduce 'sect (sets Ingr)))
        NewIngr (filter '((X) (<> (cdr X) Safe)) Ingr)
        NewSets (sets NewIngr))
    (push '*Safe Safe)
    NewSets))

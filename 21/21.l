(setq *Inp (in "21.txt" (till NIL)))
(setq *Foods (filter 'and (split *Inp "\n")))

(setq *DemoInp (in "demo.txt" (till NIL)))
(setq *DemoFoods (filter 'and (split *DemoInp "\n")))

(de parse (Food)
  (let (Data (str (pack Food))
        Ingredients (filter atom Data)
        Allergenes (cdr (last Data)))
    (for A Allergenes
      (push '*F (cons A Ingredients)))))

(de parse-all (Foods)
  (let *F '()
    (mapcar parse Foods)
    *F))

(de all-ingredients (Foods)
  (let *I '()
    (mapcar '((F) (push '*I (filter atom (str (pack F))))) Foods)
    (fish atom *I)))

(setq *Ingredients (all-ingredients *Foods))
(setq *DemoIngredients (all-ingredients *DemoFoods))

(setq *IngrSets (parse-all *Foods))
(setq *DemoIngrSets (parse-all *DemoFoods))

# 21a
(de allergenes (IngrSets)
  (uniq (mapcar car IngrSets)))

(de ingredients (IngrSets Unique)
  (uniq (fish atom (mapcar cdr IngrSets))))

(de reduce (Fun Lst Init)
  (if (= NIL Lst)
    Init
    (if (= NIL Init)
      (reduce Fun (cddr Lst) (Fun (car Lst) (cadr Lst)))
      (reduce Fun (cdr Lst) (Fun Init (car Lst))))))

(de sumup (@Allergene IngrSets)
  (macro (filter '((S) (= '@Allergene (car S))) IngrSets)))

(de sets (IngrSets)
  (mapcar cdr IngrSets))

(de narrow (Allergene IngrSets)
  (let Sets (sets (sumup Allergene IngrSets))
    (if (> (length Sets) 1)
      (reduce 'sect Sets)
      (car Sets))))

(de narrow-all (IngrSets)
  (mapcar '((A) (cons A (narrow A IngrSets))) (allergenes IngrSets)))

(setq *Candidates (narrow-all *IngrSets))
(setq *DemoCandidates (narrow-all *DemoIngrSets))

(setq *Safe (diff (ingredients *IngrSets) (ingredients *Candidates)))
(setq *DemoSafe (diff (ingredients *DemoIngrSets) (ingredients *DemoCandidates)))

(de solve-pt1 (Safe Ingredients)
  (length (sect Ingredients Safe)))

(test 5 (solve-pt1 *DemoSafe *DemoIngredients))
(prinl "count: " (solve-pt1 *Safe *Ingredients))

# 21b
(de find-dangerous (Candidates)
  (let (Cand (copy Candidates)
        *D '())
    (while Cand
      (for C (copy Cand)
        (when (= 2 (length C))
          (push '*D C)
          (setq Cand (delete C Cand))
          (setq Cand (mapcar '((X) (delete (cadr C) X)) Cand)))))
    *D))

(setq *Dangerous (find-dangerous *Candidates))
(setq *DemoDangerous (find-dangerous *DemoCandidates))

(de interwine (List1 List2)
  (make
    (loop
      (link (pop 'List1) (pop 'List2))
      (NIL List1)
      (NIL List2))))

(de canonical (Dangerous)
  (pack (head -1 (interwine (mapcar cadr (by car sort Dangerous))
                            (need (length Dangerous) ",")))))

(test "mxmxvkd,sqjhc,fvjkl" (canonical *DemoDangerous))
(prinl "canonical: " (canonical *Dangerous))

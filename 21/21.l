(setq Inp (in "21.txt" (till NIL)))
(setq Blocks (filter 'and (split Inp "\n")))

(setq DemoInp (in "demo.txt" (till NIL)))
(setq DemoBlocks (filter 'and (split DemoInp "\n")))

(class +BTree)
(dm put> (K V)
  (del> This K)
  (idx (:: D) (cons K V) T))
(dm del> (K)
  (if (lup (: D) K)
    (idx (:: D) @ NIL)))
(dm get> (K)
  (cdr (lup (: D) K)))
(dm size> ()
  (length (list> This)))
(dm bal> ()
  (bal ance (:: D) (list> This)))
(dm list> ()
  (idx (:: D)))

# 21a
(setq Ingr (new '(+BTree)))
(setq DemoIngr (new '(+BTree)))

(setq *SuspiciousIngr '())

(de parse (Food)
  (let (Line (str (pack Food))
        Ingredients (filter atom Line)
        Allergenes (cddr (filter lst? Line)))
    (!for A Allergenes
      (for I Ingredients
        (push '*SuspiciousIngr (cons A I))))))

(setq DemoList (mapcar parse DemoBlocks))

#(setq DemoList (mapcar '((X) (filter atom (str (pack X)))) DemoBlocks))
#
#(mapcar '((D)
#          (mapcar '((I)
#                    (if (get> DemoIngr I)
#                      (put> DemoIngr I (inc @))
#                      (put> DemoIngr I 1)))
#                  D))
#        DemoList)

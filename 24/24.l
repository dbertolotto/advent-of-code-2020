(de read-path (Line)
  (make
    (while Line
      (let C (pop 'Line)
        (cond
          ((or (member C '("e" "w"))) (link (intern C)))
          ((or (member C '("s" "n"))) (link (intern (pack C (pop 'Line))))))))))

(setq Inp (in "24.txt" (till)))
(setq List (mapcar read-path (filter 'and (split Inp "\n"))))

(setq DemoInp (in "demo.txt" (till)))
(setq DemoList (mapcar read-path (filter 'and (split DemoInp "\n"))))

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
  (balance (:: D) (list> This)))
(dm list> ()
  (idx (:: D)))
(dm keys> ()
  (mapcar car (list> This)))
(dm vals> ()
  (mapcar cdr (list> This)))

(de get-or-create-tile (X Y)
  (if (get> *Tiles (cons X Y))
    @
    (new '(+Tile) X Y)))

(class +Tile)
(dm T (X Y)
  (=: X X)
  (=: Y Y)
  (=: C 'w)
  (=: F NIL)
  (put> *Tiles (cons X Y) This))
(dm flip> ()
  (=: C (if (= (: C) 'w) 'b 'w)))
(dm w>  ()
  (get-or-create-tile (dec (: X)) (: Y)))
(dm e>  ()
  (get-or-create-tile (inc (: X)) (: Y)))
(dm nw> ()
  (get-or-create-tile (dec (: X)) (inc (: Y))))
(dm ne> ()
  (get-or-create-tile (: X) (inc (: Y))))
(dm sw> ()
  (get-or-create-tile (: X) (dec (: Y))))
(dm se> ()
  (get-or-create-tile (inc (: X)) (dec (: Y))))
(dm neighbors> ()
  (if (: N)
    @
    (=: N (list (w> This) (e> This)
                (nw> This) (ne> This)
                (sw> This) (se> This)))))
(dm col> ()
  (: C))
(dm should-flip> ()
  (let (Neigh (neighbors> This)
        Colors (mapcar 'col> Neigh)
        Blacks (cnt '((C) (= 'b C)) Colors))
    (cond
      ((and (= 'b (: C))
            (or (= Blacks 0) (> Blacks 2)))
         (=: F T))
      ((and (= 'w (: C))
            (= Blacks 2))
         (=: F T))
      (T (=: F NIL)))))

(de follow-path (Path)
  (let Tile (get-or-create-tile 1 1)
    (for P Path
      (setq Tile ((intern (pack P '>)) Tile)))
    (flip> Tile)))

(de count-black ()
  (cnt '((Tl) (= (get (cdr Tl) 'C) 'b))
       (list> *Tiles)))

# 24a
(setq *Tiles (new '(+BTree)))
(mapcar '((Path) (follow-path Path)) DemoList)
(test 10 (count-black))

(setq *Tiles (new '(+BTree)))
(mapcar '((Path) (follow-path Path)) List)
(prinl "black: " (count-black))

# 24b
(de grow-floor ()
  (for Tl (vals> *Tiles)
    (when (= (col> Tl) 'b)
      (neighbors> Tl))))

(de day-flip ()
  (grow-floor)
  (let Tiles (vals> *Tiles)
    (for Tl Tiles
      (should-flip> Tl))
    (for Tl Tiles
      (if (=T (get Tl 'F))
        (flip> Tl)))
    (count-black)))

# bonus: function to visualize floor
(de view-floor ()
  (grow-floor)
  (let (Tiles (list> *Tiles)
        Xs (sort (mapcar caar Tiles))
        Ys (sort (mapcar cdar Tiles))
        Ry (range (car Ys) (last Ys))
        Rx (range (car Xs) (last Xs)))
    (for I Ry
      (prin (need (- (last Ry) I) " "))
      (for J Rx
        (if (get> *Tiles (cons J I))
          (prin (uppc (get @ 'C)) " ")
          (prin ". ")))
      (prinl))))

(setq *Tiles (new '(+BTree)))
(mapcar '((Path) (follow-path Path)) DemoList)
(test 15 (day-flip))
(test 12 (day-flip))

(setq *Tiles (new '(+BTree)))
(mapcar '((Path) (follow-path Path)) DemoList)
(test 37 (for I 10 (day-flip)))

(setq *Tiles (new '(+BTree)))
(mapcar '((Path) (follow-path Path)) DemoList)
(test 2208 (prog
             (for I 100
               (prinl "Day " I ": " (day-flip))
               (if (=0 (% I 25)
                 (bal> *Tiles))))
             (count-black)))

(setq *Tiles (new '(+BTree)))
(mapcar '((Path) (follow-path Path)) List)
(for I 100
  (prinl "Day " I ": " (day-flip))
    (if (=0 (% I 25)
      (bal> *Tiles))))

(setq Inp (in "17.txt" (till NIL)))
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
  (balance (:: D) (list> This)))
(dm list> ()
  (idx (:: D)))

# 17a
(de prepare-matrix (Blocks)
  (let Matrix (new '(+BTree))
    (for (I . Row) Blocks
      (for (J . C) Row
        (put> Matrix (list I J 0) (if (= C "#") T NIL))))
    Matrix))

(de render (Matrix)
  (let (Cells (list> Matrix)
        Xs (sort (uniq (mapcar caar Cells)))
        Ys (sort (uniq (mapcar cadar Cells)))
        Zs (sort (uniq (mapcar caddar Cells))))
    (for Z Zs
      (prinl)
      (prinl "Z = " Z)
      (for X Xs
        (for Y Ys
          (prin (if (get> Matrix (list X Y Z)) "#" ".")))
        (prinl)))))

(setq Matrix (prepare-matrix Blocks))
(setq DemoMatrix (prepare-matrix DemoBlocks))

(de neighbors (Cell Matrix)
  (let ((X Y Z) (car Cell))
    (make
      (for I (range (dec X) (inc X))
        (for J (range (dec Y) (inc Y))
          (for K (range (dec Z) (inc Z))
            (let Location (list I J K)
              (when (or (<> I X) (<> J Y) (<> K Z))
                (link (cons Location (get> Matrix Location)))))))))))

(de grow (Matrix)
  (let (Cells (list> Matrix)
        Neighbors (mapcar '((C) (neighbors C Matrix)) Cells))
    (for CNs Neighbors
      (for CN CNs
        (put> Matrix (car CN) (cdr CN))))))

(de cycle (Cell Neighbors Matrix)
  (let ((Location . Active) Cell
        ActiveNeighbors (length (filter '((X) (=T (cdr X))) Neighbors)))
    (cond
      ((and Active (or (< ActiveNeighbors 2) (> ActiveNeighbors 3))) (put> Matrix Location NIL))
      ((and (not Active) (= ActiveNeighbors 3)) (put> Matrix Location T)))))

(de cycle-all (Matrix)
  (grow Matrix)
  (let (Cells (list> Matrix)
        Neighbors (mapcar '((Cell) (neighbors Cell Matrix)) Cells))
    (mapcar '((Cell Neighs) (cycle Cell Neighs Matrix)) Cells Neighbors))
  Matrix)

(de mcycle (Matrix N)
  (for I N
    (cycle-all Matrix)))

(de count-active (Matrix)
  (length (filter '((X) (=T (cdr X))) (list> Matrix))))

(test 112 (count-active (mcycle DemoMatrix 6)))

(prinl "pt1 result: " (count-active (mcycle Matrix 6)))

# 17b
(de prepare-matrix-4d (Blocks)
  (let Matrix (new '(+BTree))
    (for (I . Row) Blocks
      (for (J . C) Row
        (put> Matrix (list I J 0 0) (if (= C "#") T NIL))))
    Matrix))

(de cadddar (X)
  (car (cdr (cdr (cdr (car X))))))

(de render-4d (Matrix)
  (let (Cells (list> Matrix)
        Xs (sort (uniq (mapcar caar Cells)))
        Ys (sort (uniq (mapcar cadar Cells)))
        Zs (sort (uniq (mapcar caddar Cells)))
        Ws (sort (uniq (mapcar cadddar Cells))))
    (for Z Zs
      (for W Ws
        (prinl)
        (prinl "Z = " Z " W = " W)
        (for X Xs
          (for Y Ys
            (prin (if (get> Matrix (list X Y Z W)) "#" ".")))
          (prinl))))))

(setq Matrix4D (prepare-matrix-4d Blocks))
(setq DemoMatrix4D (prepare-matrix-4d DemoBlocks))

(de neighbors-4d (Cell Matrix)
  (let ((X Y Z W) (car Cell))
    (make
      (for I (range (dec X) (inc X))
        (for J (range (dec Y) (inc Y))
          (for K (range (dec Z) (inc Z))
            (for L (range (dec W) (inc W))
              (let Location (list I J K L)
                (when (or (<> I X) (<> J Y) (<> K Z) (<> L W))
                  (link (cons Location (get> Matrix Location))))))))))))

(de grow-4d (Matrix)
  (let (Cells (list> Matrix)
        Neighbors (mapcar '((C) (neighbors-4d C Matrix)) Cells))
    (for CNs Neighbors
      (for CN CNs
        (put> Matrix (car CN) (cdr CN))))))

(de cycle-4d (Cell Neighbors Matrix)
  (let ((Location . Active) Cell
        ActiveNeighbors (length (filter '((X) (=T (cdr X))) Neighbors)))
    (cond
      ((and Active (or (< ActiveNeighbors 2) (> ActiveNeighbors 3))) (put> Matrix Location NIL))
      ((and (not Active) (= ActiveNeighbors 3)) (put> Matrix Location T)))))

(de cycle-all-4d (Matrix)
  (grow-4d Matrix)
  (let (Cells (list> Matrix)
        Neighbors (mapcar '((Cell) (neighbors-4d Cell Matrix)) Cells))
    (mapcar '((Cell Neighs) (cycle-4d Cell Neighs Matrix)) Cells Neighbors))
  Matrix)

(de mcycle-4d (Matrix N)
  (for I N
    (cycle-all-4d Matrix)))

# the calculation takes few minutes
(test 848 (count-active (mcycle-4d DemoMatrix4D 6)))

# the calculation takes few minutes
(prinl "pt2 result: " (count-active (mcycle-4d Matrix4D 6)))

(de to-tiles (Block)
  (let Car (format (head 4 (tail 5 (car Block))))
    (cons Car (cdr Block))))

(de get-edges (Tile)
  (let (Image (cdr Tile)
        Top (car Image)
        Bottom (last Image)
        Left (mapcar 'car Image)
        Right (mapcar 'last Image))
    (list (car Tile) Top Bottom Left Right)))

(setq Inp (in "20.txt" (till NIL)))
(setq Blocks (filter 'and (split (split Inp "\n") NIL)))
(setq Tiles (mapcar 'to-tiles Blocks))
(setq Edges (mapcar 'get-edges Tiles))

(setq DemoInp (in "demo.txt" (till NIL)))
(setq DemoBlocks (filter 'and (split (split DemoInp "\n") NIL)))
(setq DemoTiles (mapcar 'to-tiles DemoBlocks))
(setq DemoEdges (mapcar 'get-edges DemoTiles))

(de matches (Edge1 Edge2)
  (let (Res NIL
        Id1 (car Edge1)
        Id2 (car Edge2))
    (when (<> Id1 Id2)
      (for I (cdr Edge1)
        (for J (cdr Edge2)
          (when (or (= I J) (= (reverse I) J))
            (setq Res (car Edge2)))
          (T Res))
        (T Res))
      Res)))

(de find-matches (Edge Edges)
  (let Res (mapcar '((E2) (matches Edge E2)) Edges)
    (cons (car Edge) (filter and Res))))

(de finder (Edges)
  (mapcar '((E1) (find-matches E1 Edges)) Edges))

(de solve-pt1 (Edges)
  (apply * (mapcar car (head 4 (by length sort (finder Edges))))))

(test 20899048083289 (solve-pt1 DemoEdges))

(prinl "pt1: " (solve-pt1 Edges))

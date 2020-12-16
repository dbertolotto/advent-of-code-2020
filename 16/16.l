(de parse-rule (Str)
  (let ((I1 _ I2) (str Str)
        (@I11 @I12) (mapcar format (split (chop (name I1)) "-"))
        (@I21 @I22) (mapcar format (split (chop (name I2)) "-")))
    (fill '((X) (or (and (>= X @I11) (<= X @I12)) (and (>= X @I21) (<= X @I22)))))))

(setq Inp (in "16.txt" (till NIL)))
(setq Blocks (split (split Inp "\n") NIL))
(setq Rules (mapcar '((X) (split X ":")) (car Blocks)))
(setq Rules (mapcar '((X) (mapcar '((Y) (pack Y)) X)) Rules))
(setq Rules (mapcar '((X) (list (car X) (parse-rule (cadr X)))) Rules))
(setq MyTicket (mapcar format (split (cadr (cadr Blocks)) ",")))
(setq NearbyTickets (mapcar '((X) (mapcar format (split X ","))) (tail -1 (caddr Blocks))))

(setq DemoInp (in "demo.txt" (till NIL)))
(setq DemoBlocks (split (split DemoInp "\n") NIL))
(setq DemoRules (mapcar '((X) (split X ":")) (car DemoBlocks)))
(setq DemoRules (mapcar '((X) (mapcar '((Y) (pack Y)) X)) DemoRules))
(setq DemoRules (mapcar '((X) (list (car X) (parse-rule (cadr X)))) DemoRules))
(setq DemoMyTicket (mapcar format (split (cadr (cadr DemoBlocks)) ",")))
(setq DemoNearbyTickets (mapcar '((X) (mapcar format (split X ","))) (tail -1 (caddr DemoBlocks))))

(setq Demo2Inp (in "demo2.txt" (till NIL)))
(setq Demo2Blocks (split (split Demo2Inp "\n") NIL))
(setq Demo2Rules (mapcar '((X) (split X ":")) (car Demo2Blocks)))
(setq Demo2Rules (mapcar '((X) (mapcar '((Y) (pack Y)) X)) Demo2Rules))
(setq Demo2Rules (mapcar '((X) (list (car X) (parse-rule (cadr X)))) Demo2Rules))
(setq Demo2MyTicket (mapcar format (split (cadr (cadr Demo2Blocks)) ",")))
(setq Demo2NearbyTickets (mapcar '((X) (mapcar format (split X ","))) (tail -1 (caddr Demo2Blocks))))

# 16a
(de check-values (Rules Tickets)
  (let Checks (mapcar cadr Rules)
    (mapcar '((Ticket) (check-ticket Ticket Checks)) Tickets)))

(de check-ticket (Ticket Checks)
  (mapcar '((Val) (if (filter =T (mapcar '((Fn) (Fn Val)) Checks)) T Val)) Ticket))

(de error-rate (Rules Tickets)
  (apply + (fish 'num? (check-values Rules Tickets))))

(test 71 (error-rate DemoRules DemoNearbyTickets))
(prinl "rate: " (error-rate Rules NearbyTickets))

# 16b
(de remove-wrong-tickets (Rules Tickets)
  (let (Filter (mapcar '((X) (not (filter num? X))) (check-values Rules Tickets)))
    (filter '((X F) F) Tickets Filter)))

(setq ValidNearbyTickets (remove-wrong-tickets Rules NearbyTickets))

(de find-fields (Rules ValidTickets)
  (let Checks (mapcar cadr Rules)
    (make
      (for I (length (car ValidTickets))
        (link (cons I (check-field (mapcar '((X) (car (nth X I))) ValidTickets) Checks)))))))

(de check-field (Field Checks)
  (mapcar '((Fn) (if (filter not (mapcar '((Val) (Fn Val)) Field)) NIL T)) Checks))

(de order-fields (Rules ValidTickets)
  (let (Map (find-fields Rules ValidTickets)
        Sorted (by '((X) (length (filter =T (cdr X)))) sort Map))
    (make
      (while Sorted
        (let L (pop 'Sorted)
          (link L)
          (setq Sorted (mapcar '((X) (cons (car X) (mapcar 'xor (cdr L) (cdr X)))) Sorted)))))))

(de field-names (Rules ValidTickets)
  (let Map (order-fields Rules ValidTickets)
    (make
      (for L Map
        (link (cons (caar (nth Rules (index T (cdr L)))) (car L)))))))

(test '(("row" . 1) ("class" . 2) ("seat" . 3)) (field-names Demo2Rules Demo2NearbyTickets))

(de solve-pt2 (Shortlist Ticket)
  (let Fields (mapcar cdr (filter '((X) (pre? "departure" (car X))) Shortlist))
    (apply * (mapcar '((X) (car (nth Ticket X))) Fields))))

(prinl "result: " (solve-pt2 (field-names Rules ValidNearbyTickets) MyTicket))


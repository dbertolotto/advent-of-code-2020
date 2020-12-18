(setq Inp (in "18.txt" (till NIL)))
(setq Blocks (filter 'and (split Inp "\n")))
(setq Expressions (mapcar '((X) (str (pack X))) Blocks))

(setq Ex1 '(1 + 2 * 3 + 4 * 5 + 6))
(setq Ex2 '(1 + (2 * 3) + (4 * (5 + 6))))
(setq Ex3 '(2 * 3 + (4 * 5)))
(setq Ex4 '(5 + (8 * 3 + 9 + 3 * 4 * 3)))
(setq Ex5 '(5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))))
(setq Ex6 '(((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2))

# 18a
(de process1 (Expr)
  (while (> (length Expr) 1)
    (let (Val1 (pop 'Expr)
          Oper (pop 'Expr)
          Val2 (pop 'Expr))
      (if (and (num? Val1) (num? Val2))
        (push 'Expr (eval (list Oper Val1 Val2)))
        (push 'Expr (eval (list Oper
                                (if (lst? Val1) (process1 Val1) Val1)
                                (if (lst? Val2) (process1 Val2) Val2)))))))
  (car Expr))

(test 71    (process1 Ex1))
(test 51    (process1 Ex2))
(test 26    (process1 Ex3))
(test 437   (process1 Ex4))
(test 12240 (process1 Ex5))
(test 13632 (process1 Ex6))

(de solve-pt1 (Expressions)
  (apply + (mapcar process1 Expressions)))

(prinl "sum: " (solve-pt1 Expressions))

# 18b
# simplified shunting-yard algorithm (https://en.wikipedia.org/wiki/Shunting-yard_algorithm)
(setq Expressions Blocks)

(setq Ex1 (chop "1 + 2 * 3 + 4 * 5 + 6"))
(setq Ex2 (chop "1 + (2 * 3) + (4 * (5 + 6))"))
(setq Ex3 (chop "2 * 3 + (4 * 5)"))
(setq Ex4 (chop "5 + (8 * 3 + 9 + 3 * 4 * 3)"))
(setq Ex5 (chop "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))
(setq Ex6 (chop "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))

# (token precedence associativity)
(setq *Tokens
  '(("+" 3 'left)
    ("*" 2 'left)))

(setq *LeftP "(")
(setq *RightP ")")

(de operators ()
  (mapcar car *Tokens))

(de has-precedence (prec T1 T2)
  (prec (cadr (assoc T1 *Tokens)) (cadr (assoc T2 *Tokens))))

(de is-associative (Side T)
  (= Side (caddr (assoc T *Tokens))))

(de process (Expr)
  (use (Out Oper)
    (while Expr
      (let Token (pop 'Expr)
        (cond
          ((num? (format Token))
            (push 'Out Token))
          ((member Token (operators))
            (while (and Oper
                        (or (has-precedence '> (car Oper) Token)
                            (and (has-precedence '= (car Oper) Token)
                                 (is-associative 'left Token)))
                        (<> (car Oper) *LeftP))
              (push 'Out (pop 'Oper)))
            (push 'Oper Token))
          ((= Token *LeftP)
            (push 'Oper Token))
          ((= Token *RightP)
            (while (<> *LeftP (car Oper))
              (push 'Out (pop 'Oper)))
            (if (= *LeftP (car Oper))
              (pop 'Oper))))))
    (while Oper
      (push 'Out (pop 'Oper)))
    (flip Out)))

(de solve-rpn (Expr)
  (use Stack
    (while Expr
      (let Token (pop 'Expr)
        (if (num? (format Token))
          (push 'Stack (format Token))
          (push 'Stack ((intern Token) (pop 'Stack) (pop 'Stack))))))
    (car Stack)))

(test 231    (solve-rpn (process Ex1)))
(test 51     (solve-rpn (process Ex2)))
(test 46     (solve-rpn (process Ex3)))
(test 1445   (solve-rpn (process Ex4)))
(test 669060 (solve-rpn (process Ex5)))
(test 23340  (solve-rpn (process Ex6)))

(de solve-pt2 (Expressions)
  (apply + (mapcar '((X) (solve-rpn (process X))) Expressions)))

(prinl "sum: " (solve-pt2 Expressions))

(setq Inp (in "19.txt" (till NIL)))
(setq Blocks (split (split Inp "\n") NIL))
(setq RawRules (car Blocks))
(setq Messages (cadr Blocks))

(setq DemoInp (in "demo.txt" (till NIL)))
(setq DemoBlocks (split (split DemoInp "\n") NIL))
(setq DemoRawRules (car DemoBlocks))
(setq DemoMessages (cadr DemoBlocks))

(setq Demo2Inp (in "demo2.txt" (till NIL)))
(setq Demo2Blocks (split (split Demo2Inp "\n") NIL))
(setq Demo2RawRules (car Demo2Blocks))
(setq Demo2Messages (cadr Demo2Blocks))

(de parse-rule (RawRule)
  (let ((Name Rule) (split RawRule ":")
        Num (format (pack Name))
        Body (str (pack Rule))
        Len (length Body)
        Fn (case Len
             (1 (let @V (car Body)
                  #(println 1 '- @V)
                  (if (str? @V)
                    (fill '(@
                      (if (= (next) @V)
                        (let Rest (rest)
                          (if Rest Rest T)))) '(@V))
                    (fill '(@
                      (let Rest (rest)
                        (check @V Rest))) '(@V)))))
             (2 (let ((@R1 @R2) Body)
                  #(println 2 '- @R1 @R2)
                  (fill '(@
                    (let Rest (rest)
                      (check @R2 (check @R1 Rest)))) '(@R1 @R2))))
             (3 (let ((@R1 @RM @R2) Body)
                  #(println 3 '- @R1 @RM @R2)
                  (if (= @RM '|)
                    (fill '(@
                      (let (Rest (rest)
                            A (check @R1 Rest)
                            B (check @R2 Rest))
                        (if (and A B) (println 3 '- T))
                        (or A B))) '(@R1 @R2))
                    (fill '(@
                      (let Rest (rest)
                        (check @R2 (check @RM (check @R1 Rest))))) '(@R1 @R2 @RM)))))
             (4 (let ((@R1 _ @R3 @R4) Body)
                  #(println 4 '- @R1 _ @R3 @R4)
                  (fill '(@
                    (let (Rest (rest)
                          A (check @R1 Rest)
                          B (check @R4 (check @R3 Rest)))
                        (if (and A B) (println 4 '- T))
                      (or A B))) '(@R1 @R3 @R4))))
             (5 (let ((@R1 @R2 _ @R3 @R4) Body)
                  #(println 5 '- @R1 @R2 _ @R3 @R4)
                  (fill '(@
                    (let Rest (rest)
                      (or (check @R2 (check @R1 Rest))
                          (check @R4 (check @R3 Rest)))
                      )) '(@R1 @R2 @R3 @R4))))
             (6 (let ((@R1 @R2 _ @R3 @R4 @R5) Body)
                  #(println 6 '- @R1 @R2 _ @R3 @R4 @R5)
                  (fill '(@
                    (let Rest (rest)
                      (or
                        (check @R2 (check @R1 Rest))
                        (check @R5 (check @R4 (check @R3 Rest)))
                        ))) '(@R1 @R2 @R3 @R4 @R5))))
             (T (quit "Don't know this rule" Body))))
    (cons Num Fn)))

(de rule (N)
  (cdr (assoc N *Rules)))

(de check (N Msg)
  #(println N Msg)
  (cond
    ((not Msg) Msg)
    (T (apply (rule N) Msg))))

(de check0 (Msg)
  (check 0 Msg))

(de count-res (Res)
  (length (filter =T Res)))

# 19a
(setq *Rules (mapcar parse-rule DemoRawRules))
(test 2 (count-res (mapcar check0 DemoMessages)))

(setq *Rules (mapcar parse-rule RawRules))
(prinl "res: " (count-res (mapcar check0 Messages)))

# 19b
#(setq *Rules (mapcar parse-rule Demo2RawRules))
#(test 3 (count-res (mapcar check0 Demo2Messages)))

#(push '*Rules (parse-rule (chop "8: 42 | 42 8")))
#(push '*Rules (parse-rule (chop "11: 42 31 | 42 11 31")))

#(test 12 (count-res (mapcar check0 Demo2Messages)))

#(prinl "res: " (count-res (mapcar check0 Messages)))

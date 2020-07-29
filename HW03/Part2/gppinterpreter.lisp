;
; CSE-341 HW02
; Emire Korkmaz
;


(setq variables (make-hash-table :test 'equal))
(setq keywords (make-hash-table :test 'equal))
; creates hash tables with the given key and values
(defun createHT(key value)
    (setq table (make-hash-table :test 'equal))
    (return-from createHT (hashtable key value table)))

; takes key, value and an empty hash table
; creates a hash table with the given parameters
; fills the give empty hash table
(defun hashtable(key value table)
    (if (null key)
        (return-from hashtable table)
        )
        (setf (gethash (car key) table) (car value))
        (hashtable (cdr key) (cdr value) table)
    )

; creates a hash table of the keywords
(defun constructor1()
    (return-from constructor1 (createHT '(( #\a #\n #\d) (#\o #\r) (#\n #\o #\t) (#\e #\q #\u #\a #\l) (#\l #\e #\s #\s) (#\n #\i #\l) (#\l #\i #\s #\t)
    (#\a #\p #\p #\e #\n #\d) ( #\c #\o #\n #\c #\a #\t) ( #\s #\e #\t) (#\d #\e #\f #\f #\u #\n) (#\f #\o #\r) (#\i #\f) (#\e #\x #\i #\t) (#\l #\o #\a #\d) (#\d #\i #\s #\p)
    (#\t #\r #\u #\e) (#\f #\a #\l #\s #\e)) '('(#\K #\W #\_ #\A #\N #\D) '(#\K #\W #\_ #\O #\R) '(#\K #\W #\_  #\N #\O #\T) '(#\K #\W #\_ #\E #\Q #\U #\A #\L)
        '(#\K #\W #\_ #\L #\E #\S #\S) '(#\K #\W #\_ #\N #\I #\L) '(#\K #\W #\_ #\L #\I #\S #\T) '(#\K #\W #\_ #\A #\P #\P #\E #\N #\D)
        '(#\K #\W #\_ #\C #\O #\N #\C #\A #\T) '(#\K #\W #\_ #\S #\E #\T) '(#\K #\W #\_ #\D #\E #\F #\F #\U #\N) '(#\K #\W #\_ #\F #\O #\R)
        '(#\K #\W #\_ #\I #\F) '(#\K #\W #\_ #\E #\X #\I #\T) '(#\K #\W #\_ #\L #\O #\A #\D) '(#\K #\W #\_ #\D #\I #\S #\P)
        '(#\K #\W #\_ #\T #\R #\U #\E) '(#\K #\W #\_ #\F #\A #\L #\S #\E))))
    )


; creates a hash table of the operators
(defun constructor2()
    (return-from constructor2 (createHT '(#\+ #\- #\/  #\* #\(  #\)  '(#\* #\*)  #\“  #\“  '#\,)
        '('( #\O #\P #\_ #\P #\L #\U #\S) '( #\O #\P #\_ #\M #\I #\N #\U #\S) '( #\O #\P #\_ #\D #\I #\V) '( #\O #\P #\_ #\M #\U #\L #\T) '( #\O #\P #\_ #\O #\P) '( #\O #\P #\_ #\C #\P)
'( #\O #\P #\_ #\D #\B #\L #\M #\U #\L #\T) '( #\O #\P #\_ #\O #\C) '(#\O #\P #\_ #\C #\C) '( #\O #\P #\_ #\C #\O #\M #\M #\A))
        ))
    )

; reads the given filename and separates them into tokens by their categories
(defun gppinterpreter(filename)
    (setq keywords (constructor1))
    (setq operators (constructor2))
    (cond ((string-equal filename "interpreter")
        (getInput)
        (return-from gppinterpreter t)
            )
        )
    (setq words (read-as-list filename))
    (setq tokens '())
    (setf pre nil)
    (interpreter words)
    )

; prints categorized tokens
(defun printTokens(tokens)
    (cond ((null tokens)
            t
        )
        (t (print (car tokens))
            (printTokens (cdr tokens))
            )
        )
    )

; traverses all the words and compares them with the given keywords, operators and value and identifier rules
; and separates them into tokens
(defun traversing(words tokens keywords operators pre)

    (cond ((null words)
            (return-from traversing tokens)
        )
        ((string-equal (categorize (car words)) "COMMENT")
            (push "COMMENT" tokens)
            (setq pre "COMMENT")
            (traversing (cdr (cdr words)) tokens keywords operators pre)
        )
        ((string-equal (categorize (car words)) "OPERATOR")
            (cond ((and (equal #\* (first (first words))) (equal #\* (second (first words))))
                    (push '( #\O #\P #\_ #\D #\B #\L #\M #\U #\L #\T) tokens)
                    (setq pre "OPERATOR")
                    (traversing (cdr (cdr words)) tokens keywords operators pre)
                ))

                (push (gethash (car (car words)) operators) tokens)
                (setq pre "OPERATOR")
                (traversing (cdr words) tokens keywords operators pre)
            )
        ((string-equal (categorize (car words)) "KEYWORD")
            (push (gethash (car words) keywords) tokens)
            (setq pre "KEYWORD")
            (traversing (cdr words) tokens keywords operators pre))
        ((= 1 (containsOnlyNums (car words)))
                (cond ((null (car words))
                    (setq pre "VALUE")
                    (traversing (cdr words) tokens keywords operators pre))
                    (t 
                        (push "VALUE" tokens)
                        (setq pre "VALUE")
                        (traversing (cdr words) tokens keywords operators pre))
                    )            
            )

        ((= 1 (containsCharandNum (car words)))
                (push "IDENTIFIER" tokens)
                (setq pre "IDENTIFIER")
                (traversing (cdr words) tokens keywords operators pre)
                
            
            )
        (t (print "Error in identifying")
            (return-from traversing nil)
            )

        )
    )

; categorizes the given words by using the rules that was determined before
(defun categorize(word)
    (cond ((or (equal word '(#\a #\n #\d)) (equal word '(#\o #\r)) (equal word '(#\n #\o #\t)) (equal word '(#\e #\q #\u #\a #\l)) (equal word '(#\l #\e #\s #\s))
               (equal word '(#\n #\i #\l)) (equal word '(#\l #\i #\s #\t)) (equal word '(#\a #\p #\p #\e #\n #\d)) (equal word '(#\c #\o #\n #\c #\a #\t)) (equal word '(#\s #\e #\t))
               (equal word '(#\d #\e #\f #\f #\u #\n)) (equal word '(#\f #\o #\r)) (equal word '(#\i #\f)) (equal word '(#\e #\x #\i #\t)) (equal word '(#\l #\o #\a #\d))
               (equal word '(#\d #\i #\s #\p)) (equal word '(#\t #\r #\u #\e)) (equal word '(#\f #\a #\l #\s #\e)))
                    (return-from categorize "KEYWORD")
            )          
          ((or (equal word  '(#\+)) (equal word '(#\-)) (equal word '(#\/) ) (equal word '(#\*)) (equal word '(#\() ) (equal word '(#\)) )
               (equal word '(#\* #\*)) (equal word  '(#\“)) (equal word  '(#\“)) (equal word  '(#\,)))
                   (return-from categorize "OPERATOR")   
            )
          ((equal word '(#\; #\;))
                    (return-from categorize "COMMENT"))
        )
    )

; this is for indentifier checking
; returns 0 if there is anything but digits or character
; returns 1 if it is valid
(defun containsCharandNum(word)
    (cond ((null word)
            1
        )
        ((equal nil (find (car word) '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
            0)
        (t (containsCharandNum (cdr word)))
    ))

; this is for value checking
; returns 0 if the given parameter contains anything but digits
; returns 1 if the given parameter is valid
(defun containsOnlyNums(word)
     (cond ((null word)
            1
        )
        ((equal nil (find (car word) '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
            0
        )
        (t (containsOnlyNums (cdr word))))
    )

; returns 1 if there is a leading zero, returns 0 otherwise
(defun isThereALeadingZero(c)
    (if (equal c #\0)
        (return-from isThereALeadingZero 1)
        (return-from isThereALeadingZero 0)

    ))

; checks if there is a leading digit for validation of identifier
; returns 0 if there is
; otherwise 1
(defun isThereALeadingDigit(c)
    (if (equal nil (find c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
        1
        0
        )
    )

; checks if the given word is a value or not
(defun isValue(word)
    (if (= 1 (isThereALeadingZero (car word)))
        (return-from isValue "Not a valid identifier")
        (containsOnlyNums word)
    )
)

; checks if the given word is an identifier or not
(defun isIdentifier(word)
    (if (= 1 (isThereALeadingDigit (car word)))
        (return-from isIdentifier 0)
        (containsCharandNum word)
    )
)

; reads the given file
(defun read-as-list (filename)
    (setq lines '())
    (setq line '())
    (with-open-file (stream filename)
    (do ((c (read-char stream nil)
               (read-char stream nil)))
        ((null c))
        (cond ((or (equal #\Newline c) (equal #\Space c))
            (setq l (reverse line))
            (push l lines)
            (setf line nil)
            (setf l nil))
            ((or (equal #\) c) (equal #\( c))
                (push line lines)
                (setq l '())
                (push c l)
                (push l lines)
                (setf line nil)
                (setf l nil)
            )
            (t (push c line)))

    ))
    (setq lines (reverse lines))
    (delete nil lines)
    (setq lines (cdr lines))
    lines
)
(defun convertKeyword(l keyword)
    (cond ((string-equal keyword "append")
        (return-from convertKeyword l)
        ))
    (cond ((null l)
        0)
        (t
            (setq x (* (digit-char-p (car l)) (power 10 (- (length l) 1))))
            (+ x (convertKeyword (cdr l) keyword))
            )
        )
    )

(defun func(operator operands)
    (cond ((= 0 (- (length operands) 1))
            (convertNum (car operands)))
        (t
        (funcall (intern operator) (convertNum  (car operands)) (func operator (cdr operands)))
        ))
    )

(defun funcKeyword (keyword operands)
    (cond ((= 0 (- (length operands) 1))
            (convertKeyword (car operands) keyword))
        (t
            (funcall (read-from-string keyword) (convertKeyword  (car operands) keyword) (funcKeyword keyword (cdr operands)))
        ))
    )

(defun andKeyword (keyword operands)
    (cond ((= 0 (- (length operands) 1))
            (convertKeyword (car operands) keyword))
        (t
            (and (convertKeyword (car operands) keyword) (funcKeyword keyword (cdr operands)))
        ))
    )

(defun orKeyword (keyword operands)
    (cond ((= 0 (- (length operands) 1))
            (convertKeyword (car operands) keyword))
        (t
            (or (convertKeyword (car operands) keyword) (funcKeyword keyword (cdr operands)))
        ))
    )

; error checkings
; counting # of paranthteses

(defun countParantheses (word op cl)
    (cond ((null word)
            (if (= op cl)
                1
                0
                )
         )
         ((equal #\( (car word)) 
            (setq op (+ op 1))
            (countParantheses (cdr word) op cl)
            )
         ((equal #\) (car word)) 
            (setq cl (+ cl 1))
            (countParantheses (cdr word) op cl)
            )
        (t (countParantheses (cdr word) op cl)))
    )

; returns 1, 2 or nil
; 1 means OPERATOR
; 2 means KEYWORD
(defun checkPossibilities(word)
    (cond ((and (equal (car word) '(#\( )) (string-equal (categorize (list (car (car (cdr word))))) "OPERATOR"))
            (return-from checkPossibilities 1)
            )
          ((and (equal (car word) '(#\( ))  (string-equal (categorize  (car (list (car (cdr word))))) "KEYWORD"))
            (return-from checkPossibilities 2)
          )
          ((string-equal (categorize (car word)) "COMMENT")
            (return-from checkPossibilities 3)
            )
          (t (return-from checkPossibilities 0))))


(defun interpreter(line)
    (cond ( (= 1 (countParantheses line 0 0))
            (cond ((= (checkPossibilities line) 1) ; operator
                    (print "SYNTAX OK.")
                    (setq operator (string (first (car (cdr line)))))
                    (setq operands (subseq line 2 (- (length line) 1)))
                    (print (func operator operands)))
                ((= (checkPossibilities line) 2)  ; keyword
                    (print "SYNTAX OKEY.")
                    (setq keyword (format nil "~{~A~}" (car (cdr line))))
                    (setq operands (subseq line 2 (- (length line) 1)))
                    (setq l (length operands))
                    (setq r (reverse (nth (- l 1) operands)))
                    (setf (nth (- l 1) operands) r)

                    (cond ((and (> (length operands) 2) (string-equal "equal" keyword))
                            (print "SYNTAX_ERROR NOT RECOGNIZED")
                            (return-from interpreter nil)
                        ))

                    (cond ((and (> (length operands) 1) (string-equal "not" keyword))
                            (print "SYNTAX_ERROR NOT RECOGNIZED")
                            (return-from interpreter nil)
                        ))
                    (cond ((and (= (length operands) 0) (string-equal "exit" keyword))
                            (print "Bye.")
                            (return-from interpreter t)
                        ))

                    (cond ((and (= (length operands) 2) (string-equal "less" keyword))
                            (print (funcall (intern "<") (convertNum  (car operands)) (func operator (cdr operands))))
                            (return-from interpreter t)
                        ))

                    (cond ((string-equal "and" keyword)
                            (print (andKeyword keyword operands))
                            (return-from interpreter t)
                        ))
                    (cond ((string-equal "deffun" keyword)
                            (cond ((= 0 (isIdentifier (car operands)))
                                    (print "VALID")
                                    (return-from interpreter t))
                                (t (print "not valid")
                            (return-from interpreter nil)))
                        ))

                    (cond ((string-equal "or" keyword)
                            (print (orKeyword keyword operands))
                            (return-from interpreter t)
                        ))

                     (cond ((string-equal "not" keyword)
                            (cond ((string-equal "nil" (car (car operands)))
                                    (print (not nil))
                                ))
                            (print (not (gethash (string (car (car operands))) variables)))
                            (return-from interpreter t)
                        ))

                    ; (setq operands (format nil "~{~A~}" (subseq line 2 (- (length line) 1)) ))
                    (print (funcKeyword keyword operands)) 
                )
                ((= (checkPossibilities line) 3) ;comment
                    (print "SYNTAX OK.")
                )
                (t (print "SYNTAX_ERROR NOT RECOGNIZED"))
                ))
            (t
                (print "SYNTAX_ERROR paranthteses")
                )
        )

    )

 (defun power (x y)
    (if (= y 0) 1
        (* x (power x (- y 1)))))


(defun convertNum(l)
    (if (equal l '(#\n #\i #\l))
        (return-from convertNum nil)
        )

    (cond ((null l)
        0)
        (t
            (setq x (* (digit-char-p (car l)) (power 10 (- (length l) 1))))
            (+ x (convertNum (cdr l)))
            )
        )
    )

(defun f(input)
    (setq lines '())
    (setq line '())
    (dolist (c input)
            (cond ((or (equal #\Newline c) (equal #\Space c))
                (setq l (reverse line))
                (push l lines)
                (setf line nil)
                (setf l nil))
                ((or (equal #\) c) (equal #\( c))
                    (push (reverse line) lines)
                    (setq l '())
                    (push c l)
                    (push l lines)
                    (setf line nil)
                    (setf l nil)
                )
                (t (push c line)))
            )
    (setq lines (reverse lines))
    (delete nil lines)
    (setq lines (cdr lines))
    lines
    )


(defun getInput()
    (terpri)
    (print "> ")
    (setf input (read-line))
    (setq input (coerce input 'list))
    (setq input (f input))
    (interpreter input)

    (return-from getInput t)

)

; !!!!!!!!!!! you should write a file's path on FILE_PATH below
(setq FILE_PATH "interpreter") 
(gppinterpreter FILE_PATH)


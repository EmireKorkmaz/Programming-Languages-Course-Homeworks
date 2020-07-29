
; CSE341 - PROGRAMMING LANGUAGES HW01

; EMIRE KORKMAZ

; related path should be given in the spellchecker functions
; usage is explained below.

(load "include.lisp") ;; "c2i and "i2c"

(defun read-as-list (filename)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
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
        	(t (push c line)))

    ))
    (setq lines1 (reverse lines))
    lines1

)

; parameters: key and document to be encoded

(defun encode (key doc)
    (setq hash (createHT '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z ) key))
    (if (null doc)
        nil
        (append (My_Encoder (car doc) 0 hash) (encode key (cdr doc)))
        )
        
    )

; Gen_Decoder_A'da decode edilen kelimenin sozlukte olup olmadigini kontorl ediyor (brute force)
(defun spell-checker-0 (word)
    (return-from spell-checker-0 (sc0helper word (read-as-list DICTIONARY_PATH))))  ; !!!!!!!!!!!!!!!!!!!!!!!!! DEGISTIRILMELI

; spell-checker-0'dan farkli olarak kelimeleri hash-table'da tuttugu icin daha hizli arama yapabiliyor.
(defun spell-checker-1 (word)

   (setq mytayble (createHT (read-as-list DICTIONARY_PATH) (read-as-list DICTIONARY_PATH))) ; !!!!!!!!!!!!!!!!!!1 DEGISTIRILMELI
   (if (gethash word mytayble)
        1
        0
    )
)

; decodes the word using word and the key taken from the Code_Breaker
(defun Gen_Decoder_A (word key)
    
        (cond ((= 0 (list-length word))
            nil
        )
        (t  ; verilen kelimenin ilk harfinin cipher alfabesinde kacinci harfe denk geldigini bulur
            (setf num (getLetterRank key (car word))) 
            (setq letter (getLetterbyNum num 1 '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z )))
            (return-from Gen_Decoder_A (cons letter (Gen_Decoder_A (cdr word) key))))))


; frequency analysis yaparak Ingilizce'deki en cok kullanilan harflerin sifreli metinde
; hangi harfe karsilik geldigini bularak 26! ihtimal yerine 20! ihtimaline dusuruyor.
(defun Gen_Decoder_B0 (doc)
    (setq popularOnes '(#\e #\t #\a #\o #\i #\n))
    (setq occurence '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (setq occurence (docOccurance doc occurence))
    (setf maxOccr (getHighestOccurance 0 0 0 occurence))
    (setq letters '())
    (setq letters (reverse (func popularOnes occurence letters 0))) ; populer harfleri degisecek kelimeyi ve dondurulecek kelimeyi parametre olarak verir
    letters
    )


; =================================================
; ================= HELPERS =======================
; =================================================


; kelimeyi ve generate edilmis cipher key'ini al
; en cok tekrar eden harfleri bul ve onlari bir hash table'a atip populer harflere map et

; a b c d e f ...
; x g j f o p ...

; (e:o, a:x) seklinde (populer harfler ve karsilik gelen cipherdaki populer harfler)

(defun call-Gen-Decoder-B (doc key)
    (setq alphabet '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
    (setq letters (Gen_Decoder_B0 doc)) ; en cok tekrar eden harfler
    (setq popularOnes '(#\e #\t #\a #\o #\i #\n))
    (setq hash (createHT popularOnes letters)) ; e:a seklinde hash'te tutuyoruz
    (checkLetters hash sortedPopular key alphabet)
    )

; l : bos liste
(defun convertC2I (letters l) 
    (if (null letters)
        (return-from convertC2I l)
        )
        (push (c2i (car letters)) l)
        (convertC2I (cdr letters) l)
    )

; l : bos liste
(defun convertITC (letters l) 
    (if (null letters)
        (return-from convertITC l)
        )
        (push (i2c (car letters)) l)
        (convertITC (cdr letters) l)
    )

; alfabede sirayla giderken eger popularOnes'daki ilk
; harfe geldiysek ve ayni zamanda ilk harfin key'deki degeri
; generate edilmis key'deki harf ise dogru yolda ilerliyoruz demektir.
; 1 dondururse bulunan key ile kalan harfleri bulmak icin brute force yapilir (Code-Breaker'da)
(defun checkLetters(hash popularOnes key alphabet)
    (setf l (car alphabet))
    (cond ((null popularOnes)
        (return-from checkLetters 1) ; buraya kadar sorunsuz geldiyse dogru alfabeyi bulduk demektir 1 dondurur
        )
        ((and (equal l (car popularOnes)) (equal (gethash l hash) (car key)) )
            (checkLetters hash (cdr popularOnes) (cdr key) (cdr alphabet))
        )
        (t (return-from checkLetters 0))
        )
    )
; sozlukte brute force arama yapmak icin yardimci fonksiyon
(defun sc0helper(word l)
    (cond ( (equal (car l) nil) 
        nil)
      ( (equal (car l) word) 
        t           
        )
      (t (sc0helper word (cdr l)))) )

; en cok tekrar eden harfleri return eder. sifreli mesajdaki
(defun func (popularOnes occurence letters i)
    (if (= (searchForNum occurence) 0)
        (return-from func letters)
        )
    (cond ((null popularOnes)
        (return-from func letters)
        ))
    (setf maxOccr (getHighestOccurance 0 0 0 occurence)) ; en cok tekrar eden harfin indexini al
    (setf (nth maxOccr occurence) 0)
    ; en cok tekrar eden harfi al (sifreli kelimedeki)
    (setf letter (nth maxOccr '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)))
    (push letter letters)

    (func (cdr popularOnes) occurence letters (+ i 1))
    )

(defun searchForNum (l) ; sifir disinda bir sey var mi diye bakar
    (cond ((null l)
            (return-from searchForNum 0)
        )
        ((> (car l) 0)
            (return-from searchForNum 1)
        )
        (t (searchForNum (cdr l)))
    )

)

; kelimedeki harflerin dagilimini bulur
(defun getOccurence (word occurence1)
    (setq num (getLetterRank '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z) (car word)))
    (cond ((null word)
        (return-from getOccurence occurence1)))
    (increase num occurence1 1)
    (getOccurence (cdr word) occurence1)
)

; verilen dokumandaki harflerin dagilimini bulur
; dokumandaki her kelime icin getOccurence fonksiyonunu cagirir
(defun docOccurance (doc occurence)
    (if (null doc)
        (return-from docOccurance occurence)
        )
        (docOccurance (cdr doc) (getOccurence (car doc) occurence))

    )
; en populer harfi bulabilmek icin kelimelerdeki dagilimda ve dokumandaki dagilimdaki
; en cok gecen harfi bulur
(defun getHighestOccurance (maxNum i j occurence) ; indexi dondur highest :D
    (cond ((null occurence)
            (return-from getHighestOccurance j)
        )
        ((> (car occurence) maxNum)
            (setf j i)
            (setf maxNum (car occurence))))

        (getHighestOccurance maxNum (+ i 1) j (cdr occurence))
    )

; kelimedeki istenilen harfleri istenilen harflerle degistirir
; word arama yapmak icin, returnedWord degisikliklerin yapildigi orijinal kelime
; index degisiklik yapilacak harfin yerini returnedWord'de bulabilmek icin kullaniliyor
(defun replaceLetters(oldLetter newLetter word returnedWord1 index wordHT)
    (cond ((null word)
        (return-from replaceLetters returnedWord1)
        )
        ((and (= 0 (gethash (car word) wordHT)) (equal oldLetter (car word)))
            (setf (gethash (car word) wordHT) 1)
            (setf (nth index returnedWord1) newLetter)
            (return-from replaceLetters returnedWord1)
        ))
        (format t "value is ~D~% car word HT is ~C~% " (gethash (car word) wordHT) (car word))

        (replaceLetters oldLetter newLetter (cdr word) returnedWord1 (+ index 1) wordHT)
    )

; harflerin dagilimi bulurken buldugumuz harfin bulundugu sayiyi artirmak icin kullanilir.
(defun increase(num occurence i)
    (cond ((= num i) ; istenilen yerdeki sayiyi artirir
        (setf x (car occurence))
        (setf x (+ x 1))
        (setf (first occurence) x)
        (return-from increase occurence)
        )
        ((= 0 (list-length occurence))
            (return-from increase nil)))
        (increase num (cdr occurence) (+ i 1))
        
    )
; en fazla bulunan harfi doner
(defun getNumofOcc (maxNum occurence2) ; en yuksek occurence'si doner

    (cond ((null occurence2)
            (return-from getNumofOcc maxNum)
        )
        ((> (car occurence2) maxNum)
            (setf maxNum (car occurence2))))

        (getNumofOcc maxNum (cdr occurence2))
    )

; Gen_Decoder_A icin yardimci fonksiyon
; kelimeleri decode eden Gen_Decoder_A'dan donen kelimeleri birlestirip butun metnin decode edilmesini saglar
(defun call-Gen-Decoder-A (doc key decodedDoc)
    (setq decodedDoc (Gen_Decoder_A (car doc) key))
    (if (null doc)
        (return-from call-Gen-Decoder-A decodedDoc)
        (return-from call-Gen-Decoder-A (cons decodedDoc (call-Gen-Decoder-A (cdr doc) key decodedDoc))))
    )

; kelimeleri tek tek kontrol ediyor eger sozlukte yoksa direkt 0 donduruyor varsa 1
; spell-checker-0 icin yardimci fonksiyon
(defun check-spells (words) ; for spell-checker-0
    (cond ((null words)
        (return-from check-spells 0)
        )
        ((spell-checker-0 (car words))
            (return-from check-spells 1)
            )
        (t (check-spells (cdr words))))
    )

; hash table'da varsa 1 yoksa 0 donduruyor
; spell-checker-1 icin yardimci fonksiyon
(defun check-spells-faster (words) ; for spell-checker-1
    (cond ((null words)
        (return-from check-spells-faster 0)
        )
        ((> (spell-checker-1 (car words)) 0)
            (return-from check-spells-faster 1)
            )
        (t (check-spells-faster (cdr words))))
    )

; generates ciphers
(defun getAlphabet(num)
    (cond ( (= num 1) 
        (return-from getAlphabet '(#\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a )))
      ( (= num 2) 
        (return-from getAlphabet '(#\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b)))
      ( (= num 3) 
        (return-from getAlphabet '(#\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c )))
      ( (= num 4) 
        (return-from getAlphabet '(#\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d )))
      ( (= num 5) 
        (return-from getAlphabet '(#\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e )))
      ( (= num 6) 
        (return-from getAlphabet '(#\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f )))
      ( (= num 7) 
        (return-from getAlphabet '(#\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g )))
      ( (= num 8) 
        (return-from getAlphabet '(#\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h )))
      ( (= num 9) 
        (return-from getAlphabet '(#\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i )))
      ( (= num 10) 
        (return-from getAlphabet '(#\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j )))
      ( (= num 11) 
        (return-from getAlphabet '(#\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k )))
      ( (= num 12) 
        (return-from getAlphabet '(#\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l )))
      ( (= num 13) 
        (return-from getAlphabet '(#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m )))
      ( (= num 14) 
        (return-from getAlphabet '(#\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n )))
      ( (= num 15) 
        (return-from getAlphabet '(#\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o )))
      ( (= num 16) 
        (return-from getAlphabet '(#\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p )))
      ( (= num 17) 
        (return-from getAlphabet '(#\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q )))
      ( (= num 18) 
        (return-from getAlphabet '(#\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r )))
      ( (= num 19) 
        (return-from getAlphabet '(#\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s )))
      ( (= num 20) 
        (return-from getAlphabet '(#\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t )))
      ( (= num 21) 
        (return-from getAlphabet '(#\v #\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u )))
      ( (= num 22) 
        (return-from getAlphabet '(#\w #\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v )))
      ( (= num 23) 
        (return-from getAlphabet '(#\x #\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w )))
      ( (= num 24) 
        (return-from getAlphabet '(#\y #\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x )))
      ( t
        (return-from getAlphabet '(#\z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y)))
      )

    )
; verilen harfin kacinci sirada oldugunu bulur
(defun getLetterRank(alphabet letter)
    (cond ((equal (car alphabet) letter)
        1)
        ((= 0 (list-length alphabet))
            0)
        (t (+ 1 (getLetterRank (cdr alphabet) letter)))
        )
    )

; istenilen siradaki harfin ne oldugunu dondurur
(defun getLetterbyNum (num i alphabet)
    (cond ((= num i)
        (return-from getLetterbyNum (car alphabet)))
        ((= 0 (list-length alphabet))
            nil)
        (t (getLetterbyNum num (+ i 1) (cdr alphabet)))
        )
    )
; hash table olusturur
; key: hash table'daki key degerleri icin
; alphabet: hash table'daki value degerleri icin
(defun createHT(key alphabet)
    (setq table (make-hash-table :test 'equal))
    (return-from createHT (hashtable key alphabet table)))

; createHT yardimci fonksiyonu
; hash table'i olusturur
(defun hashtable(key alphabet table1)
    (if (null key)
        (return-from hashtable table1)
        )
        (setf (gethash (car key) table1) (car alphabet))
        (hashtable (cdr key) (cdr alphabet) table1)
    )
; dogru key bulunduktan sonra kalan dokumani cozer
(defun changeAll (num document)
    (setq decodedDoc (Gen_Decoder_A (car document) (getAlphabet num)))
    (if (null document)
        (return-from changeAll nil)
        (return-from changeAll (cons decodedDoc (changeAll num (cdr document))))
        )

    )
; encoder fonksiyonu icin yardimci fonksiyon
; hash table'a atilmis harleri karsilik geldigi degerlerle degistirir
(defun My_Encoder(word index hash)
    (cond ((= index (list-length word))
            (return-from My_Encoder word))
        (t  
           (setf letter (nth index word))
           (setf (nth index word) (gethash letter hash))
           (My_Encoder word (+ index 1) hash)))
    )

; document: sifreli dokuman
; decoder: tercih edilen decoder fonksiyonu
; num:  iterasyonu saglayan deger her zaman 1 sayisi verilmeli
(defun Code-Breaker (document decoder num)
    (cond ((> num 24)
        (return-from Code-Breaker t)
        )
       (t   
            (setq decodedDoc '())
            (cond ((equal decoder "Gen_Decoder_A")
                    ; once ilk kelimeyi kontrol et eger kelime cikarsa dogru alfabeyi bulduk demektir devam et.
                    (setq q (Gen_Decoder_A (car document) (getAlphabet num)))
                    (cond ((spell-checker-0 q)
                        (setq decodedDoc (call-Gen-Decoder-A document (getAlphabet num) decodedDoc))
                    
                        (cond ((> (check-spells-faster decodedDoc) 0)
                            (print (changeAll num document))
                            (setf num 30) ; dogru bulunan kelimeden sonra dogru key'i ve dokumani cozer ve donguyu bitirir.
                        ))
                        )
                    )
                    )
                    ((equal decoder "Gen_Decoder_B0")
                        ;Gen_Decoder_B0 ile populer harflerin bulunfugu listeyi bulursa kalanini bulmak icin brute force ile aramak
                        ; icin Gen_Decoder_A'yi kullanir.
                        (if (= 1 (call-Gen-Decoder-B document (getAlphabet num)))
                            (Code-Breaker document "Gen_Decoder_A" 1))
                    )              
            )
    ))
    (Code-Breaker document decoder (+ num 1))
     
)
; 26! key olusturamadim kendim ufak bir liste uzerinde deneme yaptim
; buraya yukardaki listelerden herhangi birini yazinca verilen dosyadaki yaziyi decode ediyor
; sifreli metnin path'i
(setq file (read-as-list PATH)) ; !!!!!!!!!!!!! DEGISTIRILMELI

; cipher key
; verilen key'i aliyor (encode fonksiyonunda kullanmak icin)
(setq key '(#\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\a #\b #\c #\d #\e ))

; verilen metnin encode edilmesi
(print (encode key file))

; encode edilen metni dosyaya yazdiktan sonra sifreli metni okuyup decode ediyor

; yukarida encode edilen metin dosyaya yazildiktan sonra tekrar decode ediliyor 
(setq file (read-as-list PATH)) ; !!!!!!!!!!!!! DEGISTIRILMELI
(Code-Breaker (read-as-list PATH) "Gen_Decoder_A" 1) ; !!!!!!!!!!!!! DEGISTIRILMELI
(in-package :textdefrag)

;; words from: http://www.mieliestronk.com/corncob_lowercase.txt

(defvar *words* (make-hash-table :test #'equal))

(defmethod is-recognised? ((word string))
  (when (zerop (hash-table-count *words*))
    (let ((path (merge-pathnames cl-user::*WORKING-DIRECTORY* "corncob-words.txt")))
      (with-open-file (stream path)
        (loop for line = (read-line stream nil nil)
              while line do (setf (gethash line *words*) line))))
    (dolist (fragment '("as" "at" "char" "folio" "re" "mod" "he" "hi" "par" "ive")) (remhash fragment *words*)))

  (or (gethash word *words*)
      (gethash (string-downcase word) *words*)))

(defmethod is-punctuation? ((string string))
  (and (= 1 (length string))
       (is-punctuation? (char string 0))))

(defmethod is-punctuation? ((punc character))
  #+unicode-properties (unicode-punctuationp punc)
  #-unicode-properties (or (find punc "!\"#$%&'()*+,-./:;<=>?@[\]^_`{|}~") (char= (code-char 8217) punc))
  )

(defmethod is-apostrophe? ((punc character))
  (or (char= punc #\')
      (char= punc (code-char 8217))))
 
(defmethod is-apostrophe? ((word string))
  (if (= 1 (length word))
      (is-apostrophe? (char word 0))))
 
(defmethod is-letter? ((char character))
  #+unicode-properties (unicode-letterp char)
  #-unicode-properties (alpha-char-p char)
  )

(defmethod is-letter? ((string string))
  (and (= 1 (length string))
       (is-letter? (char string 0))))

(defmethod is-digit? ((char character))
  #+unicode-properties (unicode-numeralp char)
  #-unicode-properties (digit-char-p char)
  )

(defmethod is-numeralsp ((word string))
  (every #'is-digit? word))

(defmethod is-numeralsp ((word null))
  nil)

(defmethod is-sentence-terminator ((punc character))
  #+unicode-properties (unicode-sentence-terminalp punc)
  #-unicode-properties (find punc ".?!")
  )

(defmethod is-all-uppercase? ((word string))
  (every
   #+unicode-properties #'unicode-uppercase-letterp
   #-unicode-properties #'upper-case-p
   word))

(defmethod is-sentence-terminator ((word string))
  (if (= 1 (length word))
      (is-sentence-terminator (char word 0))))

(defun constituents (text)
  (loop
   with constituents = ()
   with buffer = (make-array 8 :fill-pointer 0 :adjustable t :element-type 'character)
   for char across text do
   (cond
    ((or (is-letter? char)
         (is-digit? char))
     (vector-push-extend char buffer))
    ((is-punctuation? char)
     (unless (equal "" buffer)
       (push (copy-seq buffer) constituents)
       (setf (fill-pointer buffer) 0))
     (push (string char) constituents))
    (t
     (unless (equal "" buffer)
       (push (copy-seq buffer) constituents)
       (setf (fill-pointer buffer) 0))))
   finally
   (unless (equal "" buffer)
     (push buffer constituents))
   (return (nreverse constituents))))

#|
  (ppcre:all-matches-as-strings
   #+unicode-properties "[\\p{L}\\p{N}]+|\\p{P}" ; strings of letters and digits, or punctuation
   #-unicode-properties "\\w+|\\S" ; strings of letters and digits, or non-whitespace
   text))
|#

(defun trim-spaces (text)
  (loop
   while (and (plusp (length text))
              (char= #\space (char text (1- (length text))))) do
   (decf (fill-pointer text))))

(defun defragment-punctuation (word words text)
  (cond
   ((is-apostrophe? word) ; i.e. possesive or contraction he's, Mary's, or don't
    (format text "~a~a " word (pop words)))

   ((is-punctuation? word)
    (trim-spaces text)
    (format text "~a " word))

   (t
    (format text " ~a " word)))
  
  words)

(defmethod defragment ((text string) &key verbose (recognised #'is-recognised?))
  (defragment (constituents text) :verbose verbose :recognised recognised))

(defmethod defragment ((words list) &key verbose (recognised #'is-recognised?))
  (loop
   with text = (make-array 256 :element-type 'character :fill-pointer 0 :adjustable t)
   while words
   as word = (pop words) do
   (if verbose (format t "defragment ~s~%" word))
   (cond
    ((is-punctuation? word)
     (setf words (defragment-punctuation word words text)))
         
    ((is-all-uppercase? word)
     (format text "~a" word)
     (loop as next = (first words)
           while (and next
                      (not (funcall recognised next))
                      (not (is-punctuation? next))) do
           (format text "~a" (pop words)))
     (setf words (defragment-punctuation (pop words) words text)))

    ((is-numeralsp word)
     (format text "~a" word)
     (loop while (is-numeralsp (first words)) do
           (format text "~a" (pop words)))
     ;;(break "text ~s?" text)
     ;;(setf words (defragment-punctuation (pop words) words text))
     )

    ((funcall recognised word)
     (if verbose (format t "recognised ~s~%" word))
     (cond
      ((funcall recognised (format nil "~a~a" word (first words)))
       (format text "~a~a " word (pop words)))
      (t
       (format text "~a " word)))
     (setf words (defragment-punctuation (pop words) words text)))

    (t ; now assume word is a fragment
     
     (loop with buffer = (make-array 256 :element-type 'character :fill-pointer 0 :adjustable t)
           initially (format buffer "~a" word)
           while words do

           (when (funcall recognised buffer)
             (format text "~a" buffer)
             (setf words (defragment-punctuation (pop words) words text))
             (if verbose (format t "concatenated buffer ~s text ~s~2%" buffer text))
             (return))

           (let ((lookahead (format nil "~a~a" buffer (first words))))
             (when (funcall recognised lookahead)
               (if verbose (format t "recognised lookahead ~s buffer ~s text ~s~2%" lookahead buffer text))
               (format text "~a " lookahead)
               (pop words)
               (setf words (defragment-punctuation (pop words) words text))
               (return)))

           (when (funcall recognised (first words))
             (format text "~a ~a " buffer (pop words))
             (setf words (defragment-punctuation (pop words) words text))
             (if verbose (format t "recognised buffer ~s text ~s~2%" buffer text))
             (return))

           (when (is-punctuation? (first words))
             (format text "~a" buffer)
             (setf words (defragment-punctuation (pop words) words text))
             (if verbose (format t "punctuation buffer ~s text ~s~2%" buffer text))
             (return))

           (format buffer "~a" (pop words)))))
   finally (return text)))

#|
(setq test2 "ISO standards make a posit ive contr ibution to the wor ld we l ive in. They faci l i tate trade, spread knowledge, disseminate innovative advances in technology, and share good management and conformity assessment practices.")

(defragment test2)

(setq iso-test "ISO has a membership of 163* national standards bodies f rom countr ies large and smal l, industr ia l ized, developing and in transi t ion, in a l l regions of the wor ld.")

(constituents iso-test)
(defragment iso-test)

(defragment "ISO’s por t fo l io of over 18 500* standards provides business, government and society with practical tools for a l l three dimensions of sustainable development: economic, env ironmental and socia l.")

>> "ISO’s port fo lio of over 18500* standards provides business, government and society with practical tools for a ll three dimensions of sustainable development: economic, environmental and social. "

(setq test "10 1 A U  RO C , a re a un de r th e re ce iv er  o pe ra to r ch ar ac te ris tic s cu rv e; B ay le yIII , B ay le y  Sc al es o f I nf an t  D ev el op m en t III ;   G  M FC  S, g ro ss  m ot or fu nc tio n cl as si fic at io n sy st em ; M -C H  AT , m od ifi ed c he ck lis t fo r au tis m in  t od dl er s;  N  PV , n eg at iv e pr ed ic tiv e va lu e;  P PV , p os iti ve  pr ed ic tiv e va lu e;  P  A RC  A -R , p ar en t re po rt o f c hi ld re n’ s ab ili tie s re vi se d.")

(defragment test)

>> "101A UROC, are a under th ere ce iveropera to r characteristic s curve; BayleyIII, BayleyScalesofInfan tDevelopmentIII; GMFCS, gross m ot or fu nctionclassificationsyst em; M- CHAT, modified c heck lis tforautism in t oddlers; NPV, n eg at ivepredictivevalue; PPV, positive pr edictivevalue; PARCA- R, parent re port o fchildren’s abili tie s revise d. "

|#


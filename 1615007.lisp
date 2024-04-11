; Question 1.
; This function counts the number of atoms in the nested list "L". The 
; function returns a non-negative integer. Any occurrence of NIL is not
; counted as an atom.
; Test Cases:
; (xCount '(a b c)) => 3
; (xCount '(a (b (c e)))) => 4
; (xCount '(a ())) => 1
; (xCount '()) => 0
; (xCount NIL) => 0
; (xCount '(NIL a b ())) => 2

(defun xCount (L)
    (cond
        ((null L) 0)
        ((null (car L)) (xCount (cdr L)))
        ((atom (car L)) (+ 1 (xCount (cdr L))))
        (t (+ (xCount (car L)) (xCount (cdr L))))
    )
)

(print ";-------------------- Question 1 --------------------;")
(print (xCount '(a b c)))
(print (xCount '(a (b (c d)))))
(print (xCount '(a ())))
(print (xCount '()))
(print (xCount NIL))
(print (xCount '(NIL a b ())))

; --------------------------------------------------------------------------- ;

; Question 2.
; This function takes in a non-nested list "x" and removes the duplicates in
; the list. The function will return a list with the duplicates removed. The
; function removes the duplicates in the lower positions first. For instance
; the test input (remove-duplicates (c a c)) will return (a c).
; Test Cases:
; (remove-duplicates '(a b c d e)) => (a b c d e)
; (remove-duplicates '(a a a a a)) => (a)
; (remove-duplicates '(a b c a d b)) => (c a d b)
; (remove-duplicates '()) => NIL
; (remove-duplicates NIL) => NIL

(defun remove-duplicate (x)
    (cond
        ((null x) NIL)
        ((is_in_list (car x) (cdr x)) (remove-duplicate (cdr x)))
        (t (cons (car x) (remove-duplicate (cdr x))))
    )
)

; This function checks if an atom "x" is a member of the list "L". The
; function returns true if "x" is a member of "L". The function returns NIL
; otherwise. The function will return NIL if "x" is nested within a nested
; list of "L".
; Test Cases:
; (is_in_list a '(a b c d e)) => T
; (is_in_list a '()) => NIL
; (is_in_list a NIL) => NIL
; (is_in_list a '((a))) => NIL
(defun is_in_list (x L)
    (cond
        ((null L) nil)
        ((equal x (car L)) t)
        (t (is_in_list x (cdr L)))
    )
)

(print ";-------------------- Question 2 --------------------;")
(print (remove-duplicate '(a b c d e)))
(print (remove-duplicate '(a a a a a)))
(print (remove-duplicate '(a b c a d b)))
(print (remove-duplicate '()))
(print (remove-duplicate NIL))
(print (is_in_list 'a '(a b c d e)))
(print (is_in_list 'a '()))
(print (is_in_list 'a NIL))
(print (is_in_list 'a '((a))))

; --------------------------------------------------------------------------- ;

; Question 3a.
; The function combines two lists "L1" and "L2" into a single list
; alternatingly. The function returns a single list. The function chooses
; the elements from L2 first, followed by L1. When one list is shorter than
; the other the function will append all the remaining elements from the
; longer list.
; Test Cases:
; (mix '(a b c) '(d e f)) => (d a e b f c)
; (mix '(1 2 3) '(a)) => (a 1 2 3)
; (mix '((a) (b c)) '(d e f g h)) => (d (a) e (b c) f g h)
; (mix '(1 2 3) NIL) => (1 2 3)
; (mix '(1 2 3) '(NIL)) => (NIL 1 2 3)

(defun mix (L1 L2)
    (cond
        ((NULL L2) L1)
        ((NULL L1) L2)
        (t (cons (car L2) (cons (car L1) (mix (cdr L1) (cdr L2)))))
    )
)

(print ";-------------------- Question 3a --------------------;")
(print (mix '(a b c) '(d e f)))
(print (mix '(1 2 3) '(a)))
(print (mix '((a) (b c)) '(d e f g h)))
(print (mix '(1 2 3) NIL))
(print (mix '(1 2 3) '(nil)))

; --------------------------------------------------------------------------- ;

; Question 3b.
; This function splits a list "L" into two sublists. The first sublists of the
; output contains the elements of "L" at even positions. The second sublist of
; the output contains the elements of "L" at odd positions.
; Test Cases:
; (split '(1 2 3 4 5 6)) => ((2 4 6) (1 3 5))
; (split '((a) (b c) (d e f) g h)) => (((b c) g) ((a) (d e f) h))
; (split '()) => (NIL NIL)
; (split NIL) => (NIL NIL)

(defun split (L)
    (helper-function-split '() '() L)
)

; This function splits a List "L" into two sublists as mentioned above. The
; function uses two accumulator variables "Lx" and "Ly" to store the result of
; the two sublists. The function returns a list containing the two
; sublists.
(defun helper-function-split (Lx Ly L)
    (cond
        ((null L) (list Lx Ly))
        ((null (car L)) (list Lx Ly))
        ((null (cadr L)) (list Lx (append Ly (list (car L)))))
        (t (helper-function-split (append Lx (list (cadr L))) (append Ly (list (car L))) (cddr L)))
    )
)

(print ";-------------------- Question 3b --------------------;")
(print (split '(1 2 3 4 5 6)))
(print (split '((a) (b c) (d e f) g h)))
(print (split '()))
(print (split NIL))

; --------------------------------------------------------------------------- ;

; Question 4.
; The function returns all possible subsets of size "S" in list "L". The order
; of the elements in the subset is not important. However, the subsets should
; not contain any duplicate combinations. The function returns a single list
; of all the subsets of "L".
; Test Cases:
; (subsets '(a b c) 0) => (NIL)
; (subsets '(a b c) 2) => ((a b) (a c) (b c))

(defun subsets (L S)
    (gen-subsets L S '())
)
; The function returns all possible subsets of size "S" in list "L" as
; outlined above. The function contains an accumulator parameter "acc" to 
; store the final result. The function returns a single list containing
; the subsets of "L".
(defun gen-subsets (L S acc)
    (cond
        ((= S 0) (cons acc '()))
        ((null L) NIL)
        (t (append (gen-subsets (cdr L) (- S 1) (append acc (list (car L)))) (gen-subsets (cdr L) S acc)))
    )
)

(print ";-------------------- Question 4 --------------------;")
(print (subsets '(a b c) 0))
(print (subsets '(a b c) 2))
(print (subsets '(a b c d e) 3))
(print (subsets NIL 0))
(print (subsets '(a b c) 4))

; --------------------------------------------------------------------------- ;

; Question 5.
; The function finds and replaces each occurrence of expression "E1" with
; expression "E2" in the nested list "L". The function returns a single list
; with all the substituted expressions.
; Test Cases:
; (substitute-exp 'a 'b '(a (a 2) (1 2 a))) => (b (b 2) (1 2 b))
; (substitute-exp '(a 2) 'b '(a (a 2) (1 2 a))) => (a b (1 2 a))
; (substitute-exp '(c e) '(1 2) '(a ((c e) 3) (b a) (c e) (a))) => (a ((1 2) 3) (b a) (1 2) (a))
; (substitute-exp '() '() '()) => NIL
; (substitute-exp NIL NIL NIL) => NIL

(defun substitute-exp (E1 E2 L)
    (cond
        ((null L) NIL)
        ((equal E1 (car L)) (cons E2 (substitute-exp E1 E2 (cdr L))))
        ((not (atom (car L))) (cons (substitute-exp E1 E2 (car L)) (substitute-exp E1 E2 (cdr L))))
        (t (cons (car L) (substitute-exp E1 E2 (cdr L))))
    )
)

(print ";-------------------- Question 5 --------------------;")
(print (substitute-exp 'a 'b '(a (a 2) (1 2 a))))
(print (substitute-exp '(a 2) 'b '(a (a 2) (1 2 a))))
(print (substitute-exp '(c e) '(1 2) '(a ((c e) 3) (b a) (c e) (a))))
(print (substitute-exp '() '() '()))
(print (substitute-exp NIL NIL NIL))

; --------------------------------------------------------------------------- ;

; Question 6.
; The function counts the number of unique elements in list "L". This function
; uses an approach where if car(L) is a member of cdr(L) then it drops car(L)
; from being counted. The function returns an integer representing the number
; of unique elements.
; Test Cases:
; (my-count '(a b a c d c)) => 4
; (my-count '()) => 0
; (my-count NIL) => 0

(defun my-count (L)
    (cond
        ((null L) 0)
        (t (+ (if (member (car L) (cdr L))
            0
            1
            )
            (my-count (cdr L))))
    )
)

(print ";-------------------- Question 6 --------------------;")
(print (my-count '(a b a c d c)))
(print (my-count '()))
(print (my-count NIL))

; --------------------------------------------------------------------------- ;

; Question 7a.
; The function finds all the webpages that can be reached from webpage "x".
; Each webpage "x" is represented as a string. A collection of webpage
; linkages "L" is a list of pairs. Each pair is represented as (a b) meaning
; webpage "a" has a link to webpage "b". Note that (a b) does not mean that
; webpage "b" has a link to webpage "a".
; Test Cases:
; (reached 'google '((google shopify) (google aircanada) (amazon aircanada))) => (SHOPIFY AIRCANADA)
; (reached 'google '((google shopify) (shopify amazon) (amazon google))) => (SHOPIFY AMAZON)
; (reached 'google '((google shopify) (shopify amazon) (amazon indigo))) => (SHOPIFY AMAZON INDIGO)
; (reached 'google '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google))) => (SHOPIFY AIRCANADA DELTA)
; (reached 'google' NIL) => NIL
; (reached 'google' '((shopify amazon))) => NIL
(defun reached (x L)
    (remove_root x (bfs (cons x NIL) L '()))
)

; This function takes in a webpage x and linkages L and finds all the immediate
; neighbours of x. This function returns a list of all the neighbours of
; x. This function does not check if there are duplicates in the result.
; Test Cases:
; (find_neighbours 'google '((google shopify) (google aircanada) (amazon aircanada))) => (shopify aircanada)
; (find_neighbours 'google '((google google)) (google google)) => (google google)
(defun find_neighbors (x L)
    (cond
        ((null L) NIL)
        ((equal x (caar L)) (cons (cadar L) (find_neighbors x (cdr L))))
        (t (find_neighbors x (cdr L)))
    )
)

; This function performs BFS starting from (car X). The linkages are represented
; by list L. The function keeps an accumulator variable V to store all the
; webpages that were visited by the BFS function. The function returns a
; single list with all the webpages visited by the BFS (including the root)
(defun bfs (X L V)
    (cond
        ((null X) NIL)
        ((is_in_list (car X) V) (bfs (cdr X) L V))
        (t (cons (car X)(bfs (cdr (append X (find_neighbors (car X) L))) L (append V (list (car X))))))
    )
)

; This function is to remove the webpage x from the list L. The
; reason I have this function is because the BFS function I created
; above includes in the root in the final result which is not what we
; want. This function is used simply to remove the root webpage from
; the final result.
(defun remove_root (x L)
    (cond
        ((null L) NIL)
        ((equal x (car L)) (remove_root x (cdr L)))
        (t (cons (car L) (cdr L)))
    )
)

(print ";-------------------- Question 7a --------------------;")
(print (reached "google" '(("google" "shopify") ("google" "aircanada") ("amazon" "aircanada"))))
(print (reached "google" '(("google" "shopify") ("shopify" "amazon") ("amazon" "google"))))
(print (reached "google" '(("google" "shopify") ("shopify" "amazon") ("amazon" "indigo"))))
(print (reached "google" '(("google" "shopify") ("google" "aircanada") ("amazon" "aircanada") ("aircanada" "delta") ("google" "google"))))
(print (reached "google" NIL))
(print (reached "google" '(("shopify" "amazon"))))
(print (reached "google" '(("google" "shopify") ("google" "aircanada") ("amazon" "aircanada") ("aircanada" "aircanada"))))
(print (reached "google" '(("google" "shopify") ("shopify" "google") ("google" "aircanada") ("amazon" "aircanada") ("aircanada" "aircanada") ("aircanada" "amazon"))))

; --------------------------------------------------------------------------- ;

; Question 7b.
; This function returns a permutation ranking the most referenced web pages.
; The webpages to be ranked are provided in list S. The linkages of each
; webpage are provided in list L. The function returns a single list of
; webpages ordered based on how many references they each have.
; Test Cases:
; (rank '("google" "shopify" "aircanada" "amazon") '(("google" "shopify") ("google" "aircanada") ("amazon" "aircanada"))) => ("aircanada" "shopify" "google" "amazon")
; (rank '("google" "shopify" "amazon") '(("google" "shopify") ("shopify" "amazon") ("amazon" "google"))) => ("google" "shopify" "amazon")
; (rank '("google" "shopify" "amazon" "indigo") '(("google" "shopify") ("shopify" "amazon") ("amazon" "indigo"))) => ("shopify" "amazon" "indigo" "google")
; (rank '("google" "shopify" "aircanada" "amazon" "delta") '(("google" "shopify") ("google" "aircanada") ("amazon" "aircanada") ("aircanada" "delta") ("google" "google"))) => ("aircanada" "shopify" "delta" "google" "amazon")
; (rank NIL NIL) => NIL
; (rank '("google") NIL) => ("google")
; (rank NIL '(("google" "amazon"))) => NIL
(defun rank (S L)
    (return-webpages-only (mySort (rank-helper (remove-duplicate (remove-self-looping-webpages L)) (create-acc S NIL))))
)

; This function is a helper function for the rank function. The reason I
; have this function is so that I could store the final result in an
; accumulator parameter.
(defun rank-helper (L acc)
    (cond
        ((null L) acc)
        (t (rank-helper (cdr L) (update-acc (cadar L) acc NIL)))
    )
)

; This function converts the list of web pages S into the format ((S0 R0) ...)
; where S0 is the name of the first webpage and p0 is the rank of the first
; webpage. R0 is initialized to 0.
; Test Cases:
; (create-acc '("google" "shopify" "aircanada" "amazon") NIL) => (("google" 0) ("shopify" 0) ("aircanada" 0) ("amazon" 0))
(defun create-acc (S acc)
    (cond
        ((null S) acc)
        (t (create-acc (cdr S) (append acc (list (list (car S) 0)))))
    )
)

; This function updates the accumulator variable acc with the new rankings
; of each webpage. The function takes in a webpage x and increments the
; ranking of acc by 1.
(defun update-acc (x acc acc2)
    (cond
        ((null acc) acc2)
        ((equal x (caar acc)) (update-acc x (cdr acc) (append acc2 (list (list (caar acc) (+ 1 (cadar acc)))))))
        (t (update-acc x (cdr acc) (append acc2 (list (list (caar acc) (cadar acc))))))
    )
)

; This function removes any self-looping webpages from the list of
; linkages L.
; Test Cases:
; (remove-self-looping-webpages '(("google" "google") ("amazon" "delta"))) => (("amazon" "delta"))
(defun remove-self-looping-webpages (L)
    (cond
        ((null L) NIL)
        ((equal (caar L) (cadar L)) (remove-self-looping-webpages (cdr L)))
        (t (cons (car L) (remove-self-looping-webpages (cdr L))))
    )
)

; This function takes in a list L in the format ((x0 r0) ...) where x0 is
; the webpage and r0 is the corresponding rank. The function returns a list
; of the form (x0 ...) containing only the webpages.
(defun return-webpages-only (L)
    (cond
        ((null L) NIL)
        (t (cons (caar L) (return-webpages-only (cdr L))))
    )
)

; This function sorts the list L according to a custom comparator.
(defun mySort(L)
    (sort L 'greaterThan)
)

; This function defines the comparator used to sort a list.
(defun greaterThan (L1 L2)
    (> (cadr L1) (cadr L2))
)

(print ";-------------------- Question 7b --------------------;")
(print (rank '("google" "shopify" "aircanada" "amazon") '(("google" "shopify") ("google" "aircanada") ("amazon" "aircanada"))))
(print (rank '("google" "shopify" "amazon") '(("google" "shopify") ("shopify" "amazon") ("amazon" "google"))))
(print (rank '("google" "shopify" "amazon" "indigo") '(("google" "shopify") ("shopify" "amazon") ("amazon" "indigo"))))
(print (rank '("google" "shopify" "aircanada" "amazon" "delta") '(("google" "shopify") ("google" "aircanada") ("amazon" "aircanada") ("aircanada" "delta") ("google" "google"))))
(print (rank NIL NIL))
(print (rank '("google") NIL))
(print (rank NIL '(("google" "amazon"))))
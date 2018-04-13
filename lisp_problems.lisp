;;Find the last box of a list
(defun my-last-rec (lst)
	   (cond
	     ((endp lst) (error "Empty list"))
	     ((endp (cdr lst)) lst)
	     (t (my-last-rec (cdr lst)))))
(defun my-last (lst)
  (list (car (reverse lst))))

;; Find the last two elements of a list
(defun my-but-last (lst)
  (cond
    ((endp lst) (error "Empty list"))
    ((endp (rest lst)) (error "List too short"))
    (t (last list 2))))
(defun my-but-last-rec (lst)
  (cond
    ((endp lst) (error "Empty list"))
    ((endp (cdr (cdr lst))) lst)
    (t (my-but-last-rec (cdr lst)))))
;; Find the number of elements in a list
(defun num-el (lst)
	   (cond
	     ((endp lst) 0)
	     (t (1+ (num-el (cdr lst))))))
(defun num-el-2 (lst)
	   (labels ((count-elements (lst count)
		      (cond ((endp lst) count)
			    (t (count-elements (cdr lst) (1+ count))))))
	     (count-elements lst 0)))
;;Reverse a list
(defun reverse-rec (lst)
	   (if (endp lst)
	       nil
	       (append (reverse-rec (cdr lst))
		       (cons (car lst) '()))))
(defun reverse-tail (lst)
  (labels ((rec (lst acc)
     (if (endp lst)
          acc
          (rec (cdr lst) (cons (car lst) acc)))))
      (rec lst '())))
;;Check if a list is a palindrome
(defun palindrome-p (list)
  (labels ((palstep (slow fast reve)
             (cond
               ((endp fast)        (equal slow reve))
               ((endp (cdr fast)) (equal (cdr slow) reve))
               (t                  (palstep (cdr slow)
                                            (cdr (cdr fast))
                                            (cons (car slow) reve))))))
    (palstep list list '())))
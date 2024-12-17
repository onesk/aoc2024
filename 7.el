(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-equation (line)
  (-let* (((sum-str numbers-str) (split-string line ": "))
         (sum (string-to-number sum-str))
         (numbers (->> (split-string numbers-str) (-map #'string-to-number))))
    (cons sum numbers)))

(defun parse-input (input-string)
  (->> (split-string input-string "\n" t) (-map #'parse-equation)))

(setq example '((190 10 19)
                (3267 81 40 27)
                (83 17 5)
                (156 15 6)
                (7290 6 8 6 15)
                (161011 16 10 13)
                (192 17 8 14)
                (21037 9 7 18 13)
                (292 11 6 16 20)))

(defun compute (numbers operands)
  (pcase (cons numbers operands)
    (`((,n . ,ns) . (,o . ,os)) (funcall o (compute ns os) n))
    (`((,n) . ()) n)))

(defun satisfiable-equation (ops equation)
  (-let [(sum . numbers) equation]
    (-> (apply '-table-flat (lambda (&rest operands) (compute (reverse numbers) (reverse operands)))
                (-repeat (1- (length numbers)) ops))
        (-contains? sum))))

(defun sum-claims (ops equations)
  (->> (-filter (-partial #'satisfiable-equation ops) equations)
       (-map #'car)
       (-sum)))

(defun ||| (l r)
  (string-to-number (concat (number-to-string l) (number-to-string r))))

(defun part1 (equations) (sum-claims '(+ *) equations))
(defun part2 (equations) (sum-claims '(+ * |||) equations))

(setq input (parse-input (read-input "7.txt")))
(part1 input)
(part2 input)

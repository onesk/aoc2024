(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun muls (string)
  (let ((regex "mul(\\([0-9]+\\),\\([0-9]+\\))")
        (start 0)
        positions)
    (while (string-match regex string start)
      (let ((x (string-to-number (match-string 1 string)))
            (y (string-to-number (match-string 2 string))))
        (push (list (match-beginning 0) x y) positions)
        (setq start (match-end 0))))
    (nreverse positions)))

(defun dodonts (string)
  (let ((regex "do\\(()\\|n't()\\)")
        (start 0)
        positions)
    (while (string-match regex string start)
      (push (list (match-beginning 0) (match-string 0 string)) positions)
      (setq start (match-end 0)))
    (nreverse positions)))

(defun part1 (input)
  (->> (muls input) (-map (lambda (l) (-let [(_ a b) l] (* a b)))) (-sum)))

(defun part2 (input)
  (let ((all (muls input))
        (enabled t)
        (sum 0))
    (setq all (append all (dodonts input)))
    (setq all (-sort (-on #'< #'car) all))
    (dolist (opcode all)
      (pcase opcode
        (`(,_ ,a ,b) (when enabled (incf sum (* a b))))
        (`(,_ "do()") (setq enabled t))
        (`(,_ "don't()") (setq enabled nil))))
    sum))

(setq example "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(setq example2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(setq input (read-input "3.txt"))
(part1 input)
(part2 input)

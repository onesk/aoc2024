(require 'dash)
(require 's)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (-let (((towels-str logos-str) (split-string input-string "\n\n" t) towels logos))
    (setq towels (-> towels-str (s-trim) (string-split ", " t)))
    (setq logos (-> logos-str (s-trim) (s-lines)))
    (list towels logos)))

(defun logo-ways (towels logo)
  (-let [dp (make-vector (1+ (length logo)) 0)]
    (setf (aref dp 0) 1)
    (dolist (i (-iota (length logo) 1))
      (dolist (towel towels)
       (-let [l (length towel)]
         (when (and (<= l i)
                    (equal towel (substring logo (- i l) i)))
           (cl-incf (aref dp i) (aref dp (- i l)))))))
    (aref dp (length logo))))

(defun part1 (input)
  (-let [(towels logos) input]
    (->> logos
         (-reject (-compose #'zerop (-partial #'logo-ways towels)))
         (length))))

(defun part2 (input)
  (-let [(towels logos) input]
    (->> logos
         (-map (-partial #'logo-ways towels))
         (-sum))))

(setq example (parse-input "
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"))

(setq input (parse-input (read-input "19.txt")))
(part1 input)
(part2 input)

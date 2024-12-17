(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-robot (desc)
  (-let [regex "p=\\([-0-9]+\\),\\([-0-9]+\\) v=\\([-0-9]+\\),\\([-0-9]+\\)"]
	(string-match regex desc)
	(->> (-iota 4 1)
		 (--map (match-string it desc))
		 (-map #'string-to-number))))

(defun parse-input (input-string)
  (->> (split-string input-string "\n" t)
	   (-map #'parse-robot)))

(setq example (parse-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"))

(defun move-robots (width height steps robots)
  (--map (-let [(x y vx vy) it]
		   (list (mod (+ x (* vx steps)) width)
				 (mod (+ y (* vy steps)) height)
				 vx
				 vy)) robots))

(defun quadrant (width height x y)
  (and (not (equal (1+ (* 2 x)) width))
	   (not (equal (1+ (* 2 y)) height))
	   (+ (if (< (* 2 x) width) 0 2)
		  (if (< (* 2 y) height) 0 1))))

(defun count-quadrants (width height steps robots)
  (->> (move-robots width height steps robots)
	   (-map (-lambda ((x y _ _)) (quadrant width height x y)))
	   (-frequencies)
	   (-filter #'car)
	   (-map #'cdr)
	   (-product)))

(defun make-picture (width height robots)
  (-let [bitmap (-> (lambda (_) (make-vector width ?.))
					(-map (-iota height))
					(seq-into 'vector))]
	(dolist (robot robots)
	  (-let [(x y _ _) robot]
		(setf (aref (aref bitmap y) x) ?*)))

	(dotimes (row height)
	  (message "%3s %s" row (seq-into (aref bitmap row) 'string)))

	:tree))

(defun variances (robots)
  (-let* ((sum-x1 (-> (-lambda ((x _ _ _)) x) (-map robots) (-sum)))
		  (sum-y1 (-> (-lambda ((_ y _ _)) y) (-map robots) (-sum)))
		  (sum-x2 (-> (-lambda ((x _ _ _)) (* x x)) (-map robots) (-sum)))
		  (sum-y2 (-> (-lambda ((_ y _ _)) (* y y)) (-map robots) (-sum)))
		  (n (length robots)))
	(list
	 (- (/ sum-x2 n) (expt (/ sum-x1 n) 2))
	 (- (/ sum-y2 n) (expt (/ sum-y1 n) 2)))))

(defun part1 (robots)
  (count-quadrants 101 103 100 robots))

(defun part2 (robots)
  (->> (-iota 30000)
	   (-map (lambda (steps) (cons steps (variances (move-robots 101 103 steps robots)))))
	   (-sort (-on #'< (-compose #'-product #'cdr)))
	   (-take 10)))

(setq input (parse-input (read-input "14.txt")))
(part1 input)
(part2 input)

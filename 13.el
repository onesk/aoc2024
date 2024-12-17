(require 'dash)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-machine (desc)
  (-let [regex (mapconcat #'identity '("Button A: X\\+\\([0-9]+\\), Y\\+\\([0-9]+\\)"
                                       "Button B: X\\+\\([0-9]+\\), Y\\+\\([0-9]+\\)"
                                       "Prize: X=\\([0-9]+\\), Y=\\([0-9]+\\)"
                                       ) "\n")]
    (string-match regex desc)
    (->> (-iota 6 1)
         (--map (match-string it desc))
         (-map #'string-to-number))))

(defun parse-input (input-string)
  (-let* ((machines-descs (split-string input-string "\n\n"))
          (machines (-map #'parse-machine machines-descs)))
    machines))

(defun min-tokens (machine)
  (-let [(ax ay bx by x y) machine]
    (->> (-table-flat #'list (-iota 101) (-iota 101))
         (-map (-lambda ((a b)) (list
                                (+ (* 3 a) b)
                                (- x (* ax a) (* bx b))
                                (- y (* ay a) (* by b)))))
         (-keep (-lambda ((tokens dx dy)) (and (zerop dx) (zerop dy) tokens)))
         ((lambda (tokens) (when tokens (-min tokens)))))))

(defun adjust-machine (machine)
  (-let (((ax ay bx by x y) machine)
         (delta (expt 10 13)))
    (list ax ay bx by (+ delta x) (+ delta y))))

;; ax bx | a | x
;; ay by | b | y
(defun kramer (ax ay bx by x y)
  (-let* ((det (- (* ax by) (* bx ay)))
          (a (/ (- (* x by) (* y bx)) det))
          (b (/ (- (* ax y) (* ay x)) det)))
    (list a b)))

(defun min-tokens-fast (machine)
  (-let* (((ax ay bx by x y) machine)
          ((a b) (kramer ax ay bx by x y))
          (dx (- x (* ax a) (* bx b)))
          (dy (- y (* ay a) (* by b))))
    (when (and (zerop dx) (zerop dy))
      (+ (* 3 a) b))))

(defun part1 (machines)
  (->> machines
       (-keep #'min-tokens)
       (-sum)))

(defun part2 (machines)
  (->> machines
       (-keep (-compose #'min-tokens-fast #'adjust-machine))
       (-sum)))

(setq example (parse-input
               "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"))

(setq input (parse-input (read-input "13.txt")))
(part1 input)
(part2 input)

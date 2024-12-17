(require 'dash)
(require 's)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-input (input-string)
  (let ((trimmed (string-trim input-string))
        (regexp (rx "Register A: " (group (+ digit)) "\n"
                    "Register B: " (group (+ digit)) "\n"
                    "Register C: " (group (+ digit)) "\n\n"
                    "Program: " (group (+ (any digit ?,))))))
    (when (string-match regexp trimmed)
      (-let [(a b c program) (--map (match-string it trimmed) (-iota 4 1))]
        (list (string-to-number a)
              (string-to-number b)
              (string-to-number c)
              (-> (-map #'string-to-number (split-string program ","))
                  (seq-into 'vector)))))))

(defun combo (operand a b c)
  (pcase operand
    ((or 0 1 2 3) operand)
    (4 a)
    (5 b)
    (6 c)))

(defun cpu-step (state)
  (-let* (((program ip a b c) state)
          (opcode (and (< ip (length program)) (aref program ip)))
          (operand (and (< (1+ ip) (length program)) (aref program (1+ ip))))
          output)
    (when opcode
      (unless (pcase opcode
                ;; bxl
                (1 (setq b (logxor b operand)) nil)
                ;; bst
                (2 (setq b (logand 7 (combo operand a b c))) nil)
                ;; jnz
                (3 (unless (eql a 0) (setq ip operand)))
                ;; bxc
                (4 (setq b (logxor b c)) nil)
                ;; out
                (5 (setq output (% (combo operand a b c) 8)) nil)
                ;; adv/bdv/cdv
                ((or 0 6 7)
                 (->> (/ a (expt 2 (combo operand a b c)))
                      (set (pcase opcode (0 'a) (6 'b) (7 'c)))
                      (not))))
        (setq ip (+ 2 ip)))
      (cons (list program ip a b c) output))))

(defun emulate (input)
  (-let (((a b c program) input) state next-state outputs)
    (setq state (list program 0 a b c))
    (while (setq next-state (cpu-step state))
      (setq state (car next-state))
      (when (cdr next-state) (push (cdr next-state) outputs)))
    (nreverse outputs)))

(defun quine-rec (a a-mask outputs)
  (if (null outputs) a
    (let ((b (car outputs))
          shift a-shifted next-a next-a-mask shifted-mask rec-a-min test-a-min a-min)
      (dotimes (a-limb 8)
        (catch 'continue
          (setq shift (logxor 2 a-limb)
                a-shifted (-> (logxor 5 b a-limb) (logand 7)))

          (unless (eql (logand a 7) (logand a-limb a-mask))
            (throw 'continue nil))

          (setq next-a       (logior a-limb a)
                next-a-mask  (logior a-mask 7)
                shifted-mask (lsh 7 shift))

          (unless (eql (-> next-a                (logand next-a-mask shifted-mask))
                       (-> (lsh a-shifted shift) (logand next-a-mask shifted-mask)))
            (throw 'continue nil))

          (setq next-a      (logior next-a (lsh a-shifted shift))
                next-a-mask (logior next-a-mask shifted-mask))

          (setq rec-a-min (quine-rec (lsh next-a -3)
                                     (lsh next-a-mask -3)
                                     (cdr outputs)))

          (when rec-a-min
            (setq test-a-min (logior (lsh rec-a-min 3) a-limb))
            (setq a-min (if a-min (min a-min test-a-min) test-a-min)))))

      a-min)))

(defun part1 (input)
  (->> input (emulate) (-map #'number-to-string) (s-join ",")))

(defun part2 (input)
  (-let* (((_ _ _ program) input)
          (quine-a (quine-rec 0 0 (seq-into program 'list)))
          (quine (list quine-a 0 0 program)))
    (when (equal (seq-into program 'list) (emulate quine))
      (message "it works! quine-a = %d" quine-a))))

(setq example (parse-input "
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"))

(setq quine (parse-input "Register A: 117440
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"))

(setq input (parse-input (read-input "17.txt")))
(part1 input)
(part2 input)

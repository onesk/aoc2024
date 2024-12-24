(require 'dash)
(require 's)

(defun read-input (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun parse-initial (initial-str)
  (string-match (rx (group (+ alnum)) ": " (group (+ digit))) initial-str)
  (cons (match-string 1 initial-str) (equal "1" (match-string 2 initial-str))))

(defun parse-gate (gate-str)
  (string-match (rx (group (+ alnum)) " "
                    (group (or "AND" "OR" "XOR")) " "
                    (group (+ alnum)) " -> "
                    (group (+ alnum)))
                gate-str)
  (-let [(arg1 op arg2 dest) (--map (match-string it gate-str) (-iota 4 1))]
    (list op arg1 arg2 dest)))

(defun parse-input (input-string)
  (-let (((initial-str gates-str) (split-string input-string "\n\n" t) initial gates))
    (setq initial (--> initial-str (string-split it "\n" t) (-map #'parse-initial it)))
    (setq gates (--> gates-str (string-split it "\n" t) (-map #'parse-gate it)))
    (list initial gates)))

(defun part1 (input)
  (-let (((bindings gates) input) changed)
    (catch 'break
      (while t
        (setq changed nil)
        (-each gates
          (-lambda ((op arg1 arg2 dest))
            (let ((v1 (assoc arg1 bindings))
                  (v2 (assoc arg2 bindings))
                  (vd (assoc dest bindings))
                  op-fn dest-value)
              (when (and v1 v2 (not vd))
                (setq changed t)
                (setq op-fn (pcase op
                              ("OR" (lambda (a b) (or a b)))
                              ("AND" (lambda (a b) (and a b)))
                              ("XOR" 'xor)))
                (setq dest-value (funcall op-fn (cdr v1) (cdr v2)))
                (push (cons dest dest-value) bindings)))))
        (unless changed (throw 'break nil))))
    (->> bindings
         (--keep (and (cdr it) (car it)))
         (--keep (and (s-starts-with? "z" it) (substring it 1)))
         (--map (string-to-number it))
         (--map (expt 2 it))
         (-sum))))

(defun part2 (input)
  (-let [gates (cadr input)]
    (message "digraph {")
    (-each-indexed gates
      (-lambda (idx (op arg1 arg2 dest))
        (message "%s [shape=box,label=\"%s\"]" idx op)
        (message "%s -> %s -> %s;" arg1 idx dest)
        (message "%s -> %s;" arg2 idx)))
    (message "}")
    t))

(setq example (parse-input "
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"))

(setq input (parse-input (read-input "24.txt")))
(part1 input)
(part2 input)

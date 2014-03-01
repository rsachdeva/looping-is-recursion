(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) base (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [element a-seq]
                 (if (empty? a-seq)
                   element
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [result seq1 seq2]
                 (if (or (not (= (first seq1) (first seq2))) (not (== (count seq1) (count seq2))))
                   false
                   (if (empty? seq1)
                     result
                     (recur result (rest seq1) (rest seq2)))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  ; have to bind current-seq where recur binding happens
  (loop [first-index 0
         current-seq a-seq]
    (cond
      (empty? current-seq) nil
      (pred (first current-seq)) first-index
      :else
      (recur (inc first-index) (rest current-seq)))))

(defn avg [a-seq]
  (loop [current-sum 0
         current-seq a-seq]
    (if (empty? current-seq)
      (/ current-sum (count a-seq))
      (recur (+ current-sum (first current-seq)) (rest current-seq)))))

; only for adding
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [current-seq a-seq
         odd-seq #{}]
    (if (empty? current-seq)
      odd-seq
      (recur (rest current-seq) (toggle odd-seq (first current-seq))))))

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])


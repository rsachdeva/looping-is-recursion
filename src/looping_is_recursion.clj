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
  ":(")

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])


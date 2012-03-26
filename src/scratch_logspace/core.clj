(ns scratch-logspace.core
  (:use [criterium.core :only (bench with-progress-reporting)]))

(defmacro loopn
  [accs ret]
  (let [inits (apply concat (partition 2 3 accs))
        nexts (apply concat (partition 1 3 (drop 2 accs)))]
    `(loop [c# (int 1e8)
            ~@inits]
       (if (zero? c#)
         ~ret
         (recur (dec c#) ~@nexts)))))

(defn int-add
  []
  (let [addend (int 3)]
    (loopn [acc (int 0) (+ acc addend)] acc)))

(defn float-mult
  []
  (let [multiplier (float 1.1)]
    (loopn [acc (float 1.1) (* acc multiplier)] acc)))

(defn float-mult-rand
  []
  (let [multiplier (float (* 0.1 (rand)))]
    (loopn [acc (float 1.1) (* acc multiplier)] acc)))

(defn float-mult-rand-twice
  []
  (let [m1 (float (* 0.1 (rand)))
        m2 (float (* 0.1 (rand)))]
    (loopn [acc (float 1.1) (* (* acc m1) m2)] acc)))

(defn float-mult-rand-two
  []
  (let [m1 (float (* 0.1 (rand)))
        m2 (float (* 0.1 (rand)))]
    (loopn [a1 (float 1.1) (* a1 m1)
            a2 (float 1.2) (* a2 m2)]
           (== a1 a2))))

(defn double-mult
  []
  (let [multiplier 1.1]
    (loopn [acc 1.1 (* acc multiplier)] acc)))

(defn Double-mult
  []
  (let [multiplier (Double. 1.1)]
    (loopn [acc (Double. 1.1) (* acc multiplier)] acc)))

(defn triangle
  []
  (loop [c (long 1e7)
         a (long 0)]
    (if (zero? c)
      a
      (recur (dec c) (+ a c)))))

(defn factorial
  []
  (loop [c (long 1e7)
         a (double 1)]
    (if (zero? c)
      a
      (recur (dec c) (* a c)))))

(defn -main [& args]
  (with-progress-reporting (bench (int-add)))
  (with-progress-reporting (bench (float-mult)))
  #(with-progress-reporting (bench (float-mult-rand)))
  #_(with-progress-reporting (bench (float-mult-rand-twice)))
  #_(with-progress-reporting (bench (float-mult-rand-two)))
  (with-progress-reporting (bench (double-mult)))
  (with-progress-reporting (bench (Double-mult)))
  ;; 
  #_(with-progress-reporting (bench (triangle)))
  #_(with-progress-reporting (bench (factorial))))

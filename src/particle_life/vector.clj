(ns particle-life.vector)

(defn createVector
  ([]
   (createVector 0 0))
  ([x y]
  {:x x :y y}))

(defn hashmap-map [fn hash]
  (let [keys (keys hash)
        vals (vals hash)]
    (zipmap keys (mapv fn vals))))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn sumVectors 
  ([vectors]
   (reduce sumVectors vectors))
  ([vec-a vec-b]
  (merge-with + vec-a vec-b)))

(defn subtractVectors [vec-a vec-b]
  (merge-with - vec-a vec-b))

(defn magSq [vec]
  (reduce + (mapv #(exp % 2) (vals vec))))

(defn mag [vec]
  (Math/sqrt (magSq vec)))

(defn setMag
  ([vec magnitude]
      (if (and (= (:x vec) 0) (= (:y vec) 0))
        (createVector)
        (hashmap-map #(* % magnitude) (hashmap-map #(/ % (mag vec)) vec))))
  ([vec]
   (setMag vec 1)))

(defn scalarMultiplyVector [vec n]
  (hashmap-map #(* n %) vec))
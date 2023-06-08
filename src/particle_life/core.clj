(ns particle-life.core
  (:require [particle-life.particle :refer [attracted createParticle show
                                            updateParticle tick]]
            [particle-life.vector :refer [sumVectors]]
            [quil.core :as q]
            [quil.middleware :as m])
  (:gen-class))

(defn create-quant-random [quant]
  (reduce (fn [vec _] (conj vec (dec (rand 2)))) [] (range quant)))

(def configuration 
  (let [n-colors 3]
    {:n-colors n-colors
     :quantity (reduce into (repeat n-colors [200]))
     :colors (reduce #(conj %1 (int (Math/floor (* %2 (/ 256 n-colors))))) [] (range n-colors)) 
     :attraction-matrix (reduce (fn [vec _] (conj vec (create-quant-random n-colors))) [] (range n-colors))}
    )
  )

(defn createRandomParticle [type]
  {:pre (get (range (:n-colors configuration)) type)}
  (createParticle (rand-int (q/width))
                  (rand-int (q/height))
                  {:index type :color (get (:colors configuration) type)}))

(defn setup []
  (q/frame-rate 120)
  (q/color-mode :hsb)
  {:particles (reduce #(into %1 (for [_ (range (get (:quantity configuration) %2))] (createRandomParticle %2))) 
                      '() 
                      (range (:n-colors configuration)))})

(defn accumulate-force [particle attractor]
  (let [new-acc (attracted particle attractor 
                           (get-in (:attraction-matrix configuration)
                                   [(get-in particle [:type :index])
                                    (get-in attractor [:type :index])]))]
     (assoc particle :acc (sumVectors new-acc (:acc particle)))))

(defn update-particle [particle attractors]
  ((comp tick updateParticle #(reduce accumulate-force % attractors)) particle))

(defn update-state [state]
  {:particles (let [particles (:particles state)]
    (reduce 
     (fn [updated p]
       (conj updated (update-particle p (remove #(= p %) particles)))
       )
     '()
     particles
     ))})

(defn draw-state [state]
  (q/background 255 0 255)
  (let [particles (:particles state)]
    (doseq [particle particles] (show particle))))

(defn -main []
  (q/sketch :title "Particle Life" 
            :size [400 400]
            :setup setup 
            :update update-state
            :draw draw-state
            :features [:keep-on-top]
            :middleware [m/fun-mode]))

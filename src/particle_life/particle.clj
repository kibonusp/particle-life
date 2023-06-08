(ns particle-life.particle 
  (:require [particle-life.vector :refer [createVector magSq
                                          scalarMultiplyVector setMag subtractVectors
                                          sumVectors]]
            [quil.core :as q]))

(def rmin 20)
(def rmax 10000)

(defn createParticle [x y type]
  {:pos (createVector x y)
   :vel (createVector)
   :acc (createVector)
   :type type})

(defn updateParticle [particle]
  (let [added-vel (assoc particle :vel (scalarMultiplyVector (sumVectors (:vel particle) (:acc particle)) 0.97))]
    (assoc added-vel 
           :pos (sumVectors (:pos added-vel) (:vel added-vel)) 
           :acc (scalarMultiplyVector (:acc added-vel) 0))))

(defn invert-particle [particle axis]
  (let [vel-inverted (assoc-in particle [:vel axis] (* -1 (get-in particle [:vel axis])))]
    (assoc-in vel-inverted [:acc axis] (* -1 (get-in vel-inverted [:acc axis])))))

(defn tick 
  [particle] 
  (cond 
    (< (get-in particle [:pos :x]) 0) (invert-particle particle :x) 
    (> (get-in particle [:pos :x]) (dec (q/width))) (invert-particle particle :x) 
    (< (get-in particle [:pos :y]) 0) (invert-particle particle :y) 
    (> (get-in particle [:pos :y]) (dec (q/height))) (invert-particle particle :y) 
    :else particle))

(defn show [particle] 
  (q/stroke (get-in particle [:type :color]) 255 255)
  (q/stroke-weight 4)
  ; (q/background (get-in [:type :color] particle))
  (q/point (get-in particle [:pos :x]) (get-in particle [:pos :y])))

(defn attracted [particle attractor fmax]
  (let [dir (subtractVectors (:pos attractor) (:pos particle))
        d2 (magSq dir)]
    (if (<= d2 rmin)
      (scalarMultiplyVector (setMag dir (q/map-range d2 rmin 0 0 0.2)) -1)
      (when (<= d2 rmax)
        (scalarMultiplyVector (setMag dir (q/map-range d2 rmax 0 0 0.001)) (* fmax 15))
        ))
    ))
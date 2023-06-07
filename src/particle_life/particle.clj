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
  (let [added-vel (assoc particle :vel (scalarMultiplyVector (sumVectors (:vel particle) (:acc particle)) 0.95))]
    (assoc added-vel 
           :pos (sumVectors (:pos added-vel) (:vel added-vel)) 
           :acc (scalarMultiplyVector (:acc added-vel) 0))))

(defn tick [particle] 
    (cond
      (< (get-in particle [:pos :x]) 0) (assoc-in particle [:pos :x] (dec (q/width)))
      (> (get-in particle [:pos :x]) (dec (q/width))) (assoc-in particle [:pos :x] 0)
      (< (get-in particle [:pos :y]) 0) (assoc-in particle [:pos :y] (dec (q/height)))
      (> (get-in particle [:pos :y]) (dec (q/height))) (assoc-in particle [:pos :y] 0)
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
      (scalarMultiplyVector (setMag dir (q/map-range d2 rmin 0 0 1)) -1)
      (when (<= d2 rmax)
        (scalarMultiplyVector (setMag dir (q/map-range d2 rmax 0 0 0.001)) (* fmax 15))
        ))
    ))
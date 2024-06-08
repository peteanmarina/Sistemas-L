(ns sistemas-l.core
  (:require [clojure.string :as str])
  (:gen-class))

(defrecord Turtle [x y angle pen-down? stack])

(defn init-turtle []
  (->Turtle 0.0 0.0 (* Math/PI 1.5) true []))

(defn move-forward [turtle distance]
  (let [angle (:angle turtle)
        new-x (+ (:x turtle) (* distance (Math/cos angle)))
        new-y (+ (:y turtle) (* distance (Math/sin angle)))]
    (assoc turtle :x new-x :y new-y)))

(defn turn-left [turtle angle]
  (update turtle :angle #(+ % (* angle (/ Math/PI 180.0)))))

(defn turn-right [turtle angle]
  (update turtle :angle #(- % (* angle (/ Math/PI 180.0)))))

(defn invert-direction [turtle]
  (update turtle :angle #(+ % Math/PI)))
(defn pen-up [turtle]
  (assoc turtle :pen-down? false))

(defn pen-down [turtle]
  (assoc turtle :pen-down? true))

;Falta pila

(defn parse-l-system-file [filename]
  (let [lines (str/split-lines (slurp filename))
        angle (Double/parseDouble (first lines))
        axiom (second lines)
        rules (into {}
                    (map (fn [line]
                           (let [[predecessor successor] (str/split line #" ")]
                             [predecessor successor]))
                         (drop 2 lines)))]
    {:angle angle :axiom axiom :rules rules}))

(defn apply-rules [rules symbol]
  (get rules (str symbol) (str symbol)))

(defn generate-l-system [axiom rules iterations]
  (loop [current axiom
         n iterations]
    (if (zero? n)
      current
      (recur (apply str (map #(apply-rules rules %) current)) (dec n)))))

(defn l-system-to-commands [l-system angle]
  (map (fn [char]
         (case char
           \F [:forward 1]
           \G [:forward 1]
           \f [:pen-up-forward 1]
           \g [:pen-up-forward 1]
           \+ [:right angle]
           \- [:left angle]
           \| [:invert]
           ;Falta pila
           nil))
       l-system))

(defn execute-command [turtle command]
  (let [[cmd arg] command]
    (case cmd
      :forward (move-forward turtle arg)
      :pen-up-forward (-> turtle (pen-up) (move-forward arg) (pen-down))
      :right (turn-right turtle arg)
      :left (turn-left turtle arg)
      :invert (invert-direction turtle)
      ;Falta pila
      turtle)))

(defn execute-commands [turtle commands]
  (reduce (fn [acc cmd]
            (conj acc (execute-command (last acc) cmd)))
          [turtle] commands))

(defn to-svg [turtle-positions]
  (let [lines (for [[t1 t2] (partition 2 1 turtle-positions)
                    :when (:pen-down? t1)]
                (str "<line x1=\"" (:x t1) "\" y1=\"" (:y t1)
                     "\" x2=\"" (:x t2) "\" y2=\"" (:y t2)
                     "\" stroke-width=\"0.2\" stroke=\"black\"/>"))]
    (str "<svg viewBox=\"-50 -150 300 200\" xmlns=\"http://www.w3.org/2000/svg\">"
         (str/join "" lines)
         "</svg>")))

(defn generate-fractal-svg [filename iterations output-filename]
  (let [{:keys [angle axiom rules]} (parse-l-system-file filename)
        l-system (generate-l-system axiom rules iterations)
        commands (l-system-to-commands l-system angle)
        turtle-positions (execute-commands (init-turtle) commands)
        svg-content (to-svg turtle-positions)]
    (spit output-filename svg-content)))

(defn -main [& args]
  (let [[filename iterations-str output-filename] args
        iterations (Integer/parseInt iterations-str)]
    (generate-fractal-svg filename iterations output-filename)))
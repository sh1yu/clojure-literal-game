(ns clojure-literal-game.core
  (:gen-class)
  (:require [clojure.string :as string]))


; 定义一个游戏地图
(def game-map {:living-room ["you are in the living room of a wizards house \n there is a wizard snoring loudly on the couth \n"
                             [:west     "door"     :garden]
                             [:upstairs "stairway" :attic]]
               :garden      ["you are in a beautiful garden \n there is a well in front of you \n"
                             [:east     "door"     :living-room]]
               :attic       ["you are in the attic of the wizards house \n there is a giant welding torch in the corner \n"
                             [:downstairs "stairway" :living-room]]})

; 定义场景下的物品
(def objects [:whiskey-bottle :bucket :frog :chain])

(def object-locations {:whiskey-bottle :living-room
                       :bucket         :living-room
                       :chain          :garden
                       :frog           :garden})

; 定义出生地
(def location :living-room)

;;;;;;;;定义操作
(defn describe-location [location game-map]
  (first (location game-map)))

(defn describe-path [path]
  (format "there is a %s going %s from here \n" (second path) (first path)))

(defn describe-paths [location game-map]
  (string/join "" (map describe-path (rest (get game-map location)))))

(defn is-at? [obj loc obj-loc]
  (= (obj obj-loc) loc))

(defn describe-floor [loc objs obj-loc]
  (string/join "" (map #(format "you see a %s on the floor \n" %)
                       (filter #(is-at? % loc obj-loc) objs))))

(defn look []
  (string/join "" [(describe-location location game-map)
                   (describe-paths location game-map)
                   (describe-floor location objects object-locations)]))


(defn walk-direction [direction]
  (let [next (first (filter #(= direction (first %)) (rest (location game-map))))]
    (cond next
          (do (def location (nth next 2)) (look))
          :else "you cannot go that way\n")))

(defmacro walk [direction] `(walk-direction '~direction))

(defn pickup-object [object]
  (cond (is-at? object location object-locations)
        (do
          (def object-locations (assoc object-locations object :body))
          (format "you are now carrying the %s" object))
        :else "you cannot get that."))

(defmacro pickup [object] `(pickup-object '~object))

(defn inventory []
  (filter #(is-at? % :body object-locations) objects))

(defn have? [object]
  (some #{object} (inventory)))

(def chain-welded false)
;; (defn weld [subject object]
;;   (cond (and (= location :attic)
;;              (= subject :chain)
;;              (= object :bucket)
;;              (have? :chain)
;;              (have? :bucket)
;;              (not chain-welded))
;;         (do (def chain-welded true)
;;             "the chain is now securely welded to the bucket")
;;         :else "you cannot weld like that"))

(def bucket-filled false)
;; (defn dunk [subject object]
;;   (cond (and (= location :garden)
;;              (= subject :bucket)
;;              (= object :well)
;;              (have? :bucket)
;;              chain-welded)
;;         (do (def bucket-filled true)
;;             "the bucket is now full of water")
;;         :else "you cannot dunk like that"))

(defmacro game-action [command subj obj place & args]
  `(defmacro ~command [subject# object#]
     `(cond (and (= location '~'~place)
                 (= '~subject# '~'~subj)
                 (= '~object# '~'~obj)
                 (have? '~'~subj))
            ~@'~args
            :else (format "i cannot %s like that" (name '~'~command)))))

(game-action weld :chain :bucket :attic
             (cond (and (have? :bucket) (def chain-welded true))
                   "the chain is now securely welded to the bucket"
                   :else "you do not have a bucket"))

(game-action dunk :bucket :well :garden
             (cond chain-welded
                   (do (def bucket-filled true)
                       "the bucket is now full of water")
                   :else "the water level is too low to reach"))

(game-action splash :bucket :wizard :living-room
             (cond (not bucket-filled) "the bucket has nothing in it"
                   (have? :frog) "the wizard awakens and sees that you stole
                                       his frog \n
                                       he is so upset he banishes you to the
                                       netherworlds \n you lose! the end"
                   :else "the wizard awakens from his slumber and greets you
                               warmly \n
                               he hands you the magic low-carb donut \n you win!
                               the end \n"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(ns aoc.day4
  (:import [java.security MessageDigest]
           [javax.xml.bind DatatypeConverter]))

(def secret-key "yzbqklnj")

(defonce digester
  (MessageDigest/getInstance "MD5"))

(defn md5 [s]
  (.digest digester (.getBytes s "UTF-8")))

(defn encode [key num]
  (DatatypeConverter/printHexBinary (md5 (str key num))))

(defn part-1 []
  (->> (range)
       (filter (fn [n]
                 (.startsWith (encode secret-key n) "00000")))
       first))

(defn part-2 []
  (->> (range)
       (filter (fn [n]
                 (.startsWith (encode secret-key n) "000000")))
       first))

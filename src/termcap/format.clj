(ns termcap.format
  (:refer-clojure :exclude [format])
  (:require [utils.common.parse :refer [parse-number]]))

(defn fill-string [w fill]
  (when (pos? w)
    (apply str (take w (repeat fill)))))

(defn parse-format-string [s]
  (re-find #"%(:?([0\+\#\-]?))?(\d+)?\.?(\d+)?(c|d|o|s|x|X)" s))

(def justification
  {"#" :hash
   "+" :right
   "0" :right-zero-fill
   "-" :left})

(defn format*-dispatch-fn [[_ _ _ _ _ specifier] arg]
  (keyword specifier))

(defmulti format* #'format*-dispatch-fn)

(defmethod format* :c [[_ _ alignment width precision _] arg]
  (clojure.core/format "%c" (char (parse-number arg))))

(defmethod format* :d [[_ _ alignment width precision _] arg]
  (let [as-str (str arg)
        argw (count as-str)
        width (parse-number (or width argw))
        precision (max (parse-number (or precision argw)) argw)
        value (if (> precision argw) (str (fill-string (- precision argw) \0) as-str) as-str)
        valw (count value)]
    (case (justification alignment)
      :left (str value (fill-string (- width valw) \space))
      :right-zero-fill (str (fill-string (- width valw) \0) value)
      (str (fill-string (- width valw) \space) value))))

(defmethod format* :o [[_ _ alignment width precision _] arg]
  (let [as-str (clojure.core/format "%o" arg)
        argw (count as-str)
        width (parse-number (or width argw))
        precision (max (parse-number (or precision argw)) argw)
        value (if (> precision argw) (str (fill-string (- precision argw) \0) as-str) as-str)
        valw (count value)]
    (case (justification alignment)
      :left (str value (fill-string (- width valw) \space))
      :right-zero-fill (str (fill-string (- width valw) \0) value)
      :hash (str (fill-string (- width 1 valw) \space) "0" value)
      (str (fill-string (- width valw) \space) value))))

(defmethod format* :s [[_ _ alignment width precision _] arg]
  (let [as-str (str arg)
        argw (count as-str)
        width (parse-number (or width argw))
        precision (min (parse-number (or precision argw)) argw)
        value (apply str (take precision as-str))
        valw (count value)]
    (case (justification alignment)
      :left (str value (fill-string (- width valw) \space))
      :right-zero-fill (str (fill-string (- width valw) \0) value)
      (str (fill-string (- width valw) \space) value))))

(defmethod format* :x [[_ _ alignment width precision _] arg]
  (let [as-str (clojure.core/format "%x" arg)
        argw (count as-str)
        width (parse-number (or width argw))
        precision (max (parse-number (or precision argw)) argw)
        value (if (> precision argw) (str (fill-string (- precision argw) \0) as-str) as-str)
        valw (count value)]
    (case (justification alignment)
      :left (str value (fill-string (- width valw) \space))
      :right-zero-fill (str (fill-string (- width valw) \0) value)
      :hash (str "0x" (fill-string (- width 2 valw) \0) value)
      (str (fill-string (- width valw) \space) value))))

(defmethod format* :X [[_ _ alignment width precision _] arg]
  (let [as-str (clojure.core/format "%X" arg)
        argw (count as-str)
        width (parse-number (or width argw))
        precision (max (parse-number (or precision argw)) argw)
        value (if (> precision argw) (str (fill-string (- precision argw) \0) as-str) as-str)
        valw (count value)]
    (case (justification alignment)
      :left (str value (fill-string (- width valw) \space))
      :right-zero-fill (str (fill-string (- width valw) \0) value)
      :hash (str "0X" (fill-string (- width 2 valw) \0) value)
      (str (fill-string (- width valw) \space) value))))

(defn format [s value]
  (let [how (parse-format-string s)]
    (format* how value)))

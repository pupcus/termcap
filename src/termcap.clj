(ns termcap
  (:require [termcap.db :as db]
            [termcap.mapping :as mapping]
            [termcap.parse :as parse]
            [termcap.utils :as u]))

(defonce terminal (jline.TerminalFactory/get))

(defn tgetent
  ([] (tgetent (u/get-term)))
  ([term]
   (db/get-termcap-entry term)))

(defn tgetnum
  ([cap]
   (tgetnum cap (u/get-term)))
  ([cap term]
   (let [lookup (if (keyword? cap) cap (keyword cap))]
     (or (get-in (tgetent term) [:numbers lookup])
         (get-in (tgetent term) [:offsets lookup])))))

(defn tgetflag
  ([cap]
   (tgetflag cap (u/get-term)))
  ([cap term]
   (let [lookup (if (keyword? cap) cap (keyword cap))]
     (get-in (tgetent term) [:booleans lookup]))))

(defn tgetstr
  ([cap]
   (tgetstr cap (u/get-term)))
  ([cap term]
   (let [lookup (if (keyword? cap) cap (keyword cap))]
     (get-in (tgetent term) [:table lookup]))))

(defn tget
  ([cap]
   (tget cap (u/get-term)))
  ([cap term]
   (let [cap (if (keyword? cap) cap (keyword cap))
         cap (get mapping/cap-lookup cap cap)]
      (or (tgetstr cap term)
          (tgetnum cap term)
          (tgetflag cap term)
          :not-found))))

(defn tparm [cap & args]
  (let [cap (tget cap)
        [r s v a] (apply parse/parse cap [] {} args)]
    r))

(defn tgoto [hpos vpos]
  (print (tparm :cursor_address (int hpos) (int vpos))))

(defn tputs [cap]
  (print (tget cap)))


(defn getWidth []
  (let [columns (tget :columns)
        columns (if (= columns :not-found) 0 columns)]
    (max (.getWidth terminal) columns)))

(defn getHeight []
  (let [lines (tget :lines)
        lines (if (= lines :not-found) 0 lines)]
    (max (.getHeight terminal) lines)))

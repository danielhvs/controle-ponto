(ns ponto.core
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer :all]
            [clojure.string :as str]
)
  (:gen-class))

(def meses [:jan :fev :mar :abr :mai :jun :jul :ago :set :out :nov :dez])

(defn in->bigint [pergunta]
  (println pergunta)
  (bigint (read-line)))

(defn abs [x]
  (if (> x 0) x (- x)))

(defn salva [arquivo conteudo]
  (-> (str "resources/" arquivo)
      (spit conteudo)))

(defn carrega [arquivo]
  (-> (str "resources/" arquivo)
      (slurp)
      (edn/read-string)))

(defn adiciona-hora [mapa mes dia minutos]
  (assoc-in mapa [mes (dec dia)] minutos))

(defn formata-hora [minutos]
  (let [horas (quot minutos 60)
        minutos-restantes (-> minutos (- (-> 60 (* horas))))]
    (if (-> horas (not= 0))
        (str horas "h" (abs minutos-restantes) "m ou " minutos "m")
        (str minutos-restantes "min"))))

(defn sumariza [mapa]
  (for [mes meses]
    {mes (formata-hora (reduce + (mes mapa)))}))

(defn sumario [mapa & args]
  (pprint (sumariza mapa)))

(defn second-arg->keyword [args]
  (->> args (first) (second) (keyword)))

(defn horas [mapa & args]
  (let [mes (second-arg->keyword args)
        resultado (map-indexed #(vector (inc %1) (formata-hora %2)) (mes mapa))]
    (pprint resultado)))

(defn gravar [mapa & args]
  (let [mes (second-arg->keyword args)
        dia (in->bigint "dia: ")
        minutos (in->bigint "minutos: ")
        ponto (adiciona-hora mapa mes dia minutos)]
    (pprint (sumariza ponto))
    (salva "ponto.map" ponto)))

(defn resumo [mapa & args]
  (let [res (for [mes meses] (reduce +  (mes mapa)))] 
    (pprint (formata-hora (reduce + res)))))


(def operacoes {:+ + :- -})

(defn le-operacao [op]
  ((keyword op) operacoes))

(defn le-hora [p]
  (let [horas (cond 
               (str/includes? p "h") (first (str/split p #"h"))
               :else "0") 
        minutos (cond 
                 (and (str/includes? p "m") (str/includes? p "h")) (first (str/split (second (str/split p #"h")) #"m")) 
                 (str/includes? p "m")  (first (str/split p #"m")) 
                 (and (not (str/includes? p "m")) (str/includes? p "h")) (second (str/split p #"h")) 
                 (and (not (str/includes? p "m")) (not (str/includes? p "h"))) p
                 :else "0")] 
    (+
     (* 60 (read-string horas))
     (if minutos 
       (read-string minutos) 
       0))))

(defn calcula "Obrigatorio ser no formato 3h23m, ou seja, com h e m"
[mapa & args]
  (let [p1 (le-hora (nth (first args) 1))
        op (le-operacao (nth (first args) 2))
        p2 (le-hora (nth (first args) 3))]
    (pprint (formata-hora (op p1 p2)))))

(def funcionalidades
  {:sumario sumario
   :horas horas
   :resumo resumo
   :gravar gravar
   :calcula calcula})

(defn -main [& args]
  (if-let [key-funcao (keyword (first args))]
    (if-let [funcao (key-funcao funcionalidades)]
      (let [mapa (carrega "ponto.map")]
        (funcao mapa args))
      (pprint funcionalidades))
    (pprint funcionalidades)))

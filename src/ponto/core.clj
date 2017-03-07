(ns ponto.core
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer :all])
  (:gen-class))

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
        (str horas "h" (abs minutos-restantes) "min")
        (str minutos-restantes "min"))))

(defn sumariza [mapa]
  (for [mes [:jan :fev :mar :abr :mai :jun :jul :ago :set :out :nov :dez]]
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

(def funcionalidades
  {:sumario sumario
   :horas horas
   :gravar gravar})

(defn -main [& args]
  (if-let [key-funcao (keyword (first args))]
    (if-let [funcao (key-funcao funcionalidades)]
      (let [mapa (carrega "ponto.map")]
        (funcao mapa args))
      (pprint funcionalidades))
    (pprint funcionalidades)))

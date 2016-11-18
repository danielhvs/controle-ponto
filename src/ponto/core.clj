(ns ponto.core
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer :all])
  (:gen-class))

(defn in->bigint [pergunta]
  (println pergunta)
  (bigint (read-line)))

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
        minutos-restantes (- minutos (* 60 horas))]
    (if (> horas 0)
        (str horas "h" minutos-restantes "min")
        (str minutos-restantes "min"))))

(defn sumariza [mapa]
  (for [mes [:jan :fev :mar :abr :mai :jun :jul :ago :set :out :nov :dez]]
    {mes (formata-hora (reduce + (mes mapa)))}))

(defn -main [& args]
  (let [mes (keyword (first args))
        dia (in->bigint "dia: ")
        minutos (in->bigint "minutos: ")
        mapa (carrega "ponto.map")
        ponto (adiciona-hora mapa mes dia minutos)]
    (pprint (sumariza ponto))
    (salva "ponto.map" ponto)))

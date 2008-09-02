(in-package b-user)

(include b/biochem :use)

(def-species-types compartment
  (mapkkk  :documentation "unphosphorylated mapkkk")
  (mapkkk* :documentation "phosphorylated mapkkk")
  (mapkk   :documentation "unphosphorylated mapkk")
  (mapkk*  :documentation "phosphorylated mapkk")
  (mapk    :documentation "unphosphorylated mapk")
  (mapk*   :documentation "phosphorylated mapk"))


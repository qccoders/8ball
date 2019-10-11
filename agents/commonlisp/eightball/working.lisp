(in-package #:eightball)




(let ((question "why?") (ttl 5))
    (->> (drakma:http-request "http://localhost:8080/answer?q=why&ttl=5")
         (nth-value 0)
         (flexi-streams:octets-to-string)
         (spy))
    )